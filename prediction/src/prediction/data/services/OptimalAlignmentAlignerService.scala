package prediction.data.services

import cats._
import cats.implicits._

import prediction.domain.entities.algorithm._
import prediction.domain.usecases._

import prediction.data.ports.services._

final class OptimalAlignmentAlignerService[F[_]: Monad: Parallel, Matrix[_], Vec[_]](
    matrix: MatrixServicePort[F, Matrix, AlgorithmScore],
    vector: VectorServicePort[F, Vec, AlgorithmToken],
    scoreCalculator: ScoreCalculatorUseCase[F, AlgorithmToken, AlgorithmScore],
    alignmentFinder: AlignmentFinderUseCase[F, Matrix, AlgorithmScore],
    optimalAlignment: OptimalAlignmentBuilderUseCase[F, AlgorithmSequence, Matrix, AlgorithmScore]
) extends OptimalAlignmentAlignerUseCase[F, AlgorithmSequence, AlgorithmAlignment] {

  type Sequence = Vec[AlgorithmToken]

  // TODO rethink this design
  case class Alignments(s: Sequence, t: Sequence)
  case class Sequences(left: AlgorithmSequence, right: AlgorithmSequence)

  override def align(left: AlgorithmSequence, right: AlgorithmSequence): F[AlgorithmAlignment] =
    for {
      m          <- optimalAlignment.build(left, right)
      size       <- Applicative[F].pure(left.size * right.size)
      alignments <- createEmptyAlignments(size)
      sequences  <- wrapSequences(left, right)
      alignment  <- startingStep(m) >>=
                      consumeTailSequence(m, alignments, sequences) >>=
                      buildBestAlignments(m, alignments, sequences) >>=
                      consumeRemainingTokens(alignments, sequences) >>=
                      buildSequenceAlignment(alignments)
    } yield alignment

  def createEmptyAlignments(size: Int): F[Alignments] =
    (vector.empty(size), vector.empty(size)).parMapN {
      case (s, t) => Alignments(s, t)
    }

  def wrapSequences(left: AlgorithmSequence, right: AlgorithmSequence): F[Sequences] =
    Applicative[F].pure(Sequences(left, right))

  def startingStep(m: Matrix[AlgorithmScore]): F[AlgorithmStep] =
    alignmentFinder.findCoordinates(m).map {
      case (i, j) => AlgorithmStep(i, j)
    }

  def consumeTailSequence(m: Matrix[AlgorithmScore], alignments: Alignments, sequences: Sequences)(
      step: AlgorithmStep
  ): F[AlgorithmStep] = {
    def go(current: AlgorithmStep): F[AlgorithmStep] =
      Applicative[F].pure(current.i === step.i).flatMap {
        case true  => Applicative[F].pure(current)
        case false => consumeUp(alignments, sequences)(current) >>= go
      }

    matrix.size(m).flatMap {
      // TODO rethink this biased design
      case (rows, _) => go(AlgorithmStep(rows - 1, step.j))
    }
  }

  /**
    * Build both sequences, choosing alignment that best scores between smaller prefixes already stored.
    *
    * At each iteration check from where the current value was chosen:
    *  - UpLeft -> choose both sequences
    *  - Left   -> choose the right sequence and blank on left
    *  - Up     -> choose the left sequence and blank on right
    *
    * and decide when consuming token from sequences to the new alignment.
    *
    * @param m Matrix of Optimal Alignments
    * @param alignment container with `s` and `t` sequences,
    * representing `left` and `right` aligned sequences, respectively
    * @param sequences container with `left` and `right` original sequences
    * @param step container with current valid indexes
    * @return new step with indexes modified
    */
  def buildBestAlignments(m: Matrix[AlgorithmScore], alignments: Alignments, sequences: Sequences)(
      step: AlgorithmStep
  ): F[AlgorithmStep] = {
    def go(step: AlgorithmStep): F[AlgorithmStep] = {
      def calculate(step: AlgorithmStep): F[AlgorithmStep] = {
        def sum(f: F[AlgorithmScore], g: F[AlgorithmScore]): F[AlgorithmScore] =
          (f, g).parMapN { case (f, g) => f + g }

        import AlgorithmToken._
        import sequences._, step._

        // format: off
        val UP      = sum(matrix.get(m, i - 1, j    ), scoreCalculator.calculate(left.at(i), Empty      ))
        val UP_LEFT = sum(matrix.get(m, i - 1, j - 1), scoreCalculator.calculate(left.at(i), right.at(j)))
        val LEFT    = sum(matrix.get(m, i    , j - 1), scoreCalculator.calculate(Empty     , right.at(j)))
        // format: on

        matrix.get(m, i, j).flatMap { target =>
          (UP, UP_LEFT, LEFT).parTupled.flatMap {
            // format: off
            case (_       , `target`, _       ) => consumeUpLeft(alignments, sequences)(step) >>= go
            case (_       , _       , `target`) =>   consumeLeft(alignments, sequences)(step) >>= go
            case (`target`, _       , _       ) =>     consumeUp(alignments, sequences)(step) >>= go
            // format: on
          }
        }
      }

      import step._
      Applicative[F].pure(i =!= 0 && j =!= 0).flatMap {
        case true  => calculate(step)
        case false => Applicative[F].pure(step)
      }
    }

    go(step)
  }

  /**
    * The algorithm stops when reaches first row or first column.
    *
    * To build the entire sequence, it need to consume the rest of the remaining sequence.
    *
    * @param alignment container with `s` and `t` sequences,
    * representing `left` and `right` aligned sequences, respectively
    * @param sequences container with `left` and `right` original sequences
    * @param step container with current valid indexes
    * @return new step with indexes modified
    */
  def consumeRemainingTokens(alignments: Alignments, sequences: Sequences)(step: AlgorithmStep): F[AlgorithmStep] = {
    def go(step: AlgorithmStep): F[AlgorithmStep] =
      Applicative[F].pure(step.i -> step.j).flatMap {
        // format: off
        case (0, 0) => Applicative[F].pure(step)
        case (_, 0) =>   consumeUp(alignments, sequences)(step) >>= go
        case (0, _) => consumeLeft(alignments, sequences)(step) >>= go
        // format: on
      }

    go(step)
  }

  /**
    * The algorithm builds sequences bottom-up, it need to reverse to construct the correct sequence.
    *
    * The sequence could be of `left.size * right.size`, but only `step.k` is used at end of execution.
    * Take only `step.k` elements and discard the rest.
    *
    * @param alignment container with `s` and `t` sequences,
    * representing `left` and `right` aligned sequences, respectively
    * @param step container with current valid indexes
    * @return both sequences best aligned
    */
  def buildSequenceAlignment(alignments: Alignments)(step: AlgorithmStep): F[AlgorithmAlignment] = {
    def reverseApply(vec: Sequence): F[AlgorithmSequence] =
      vector.toScalaVector(vec).map(_.take(step.k)).map(_.reverse).map(AlgorithmSequence.apply)

    import alignments._
    (reverseApply(s), reverseApply(t))
      .parMapN(AlgorithmAlignment.apply)
  }

  // -----

  /**
    * Consuming 'Up' in the Optimal Alignment Matrix means that choosing the `left` sequence best scores
    * instead of aligning with `right` sequence.
    *
    * @param alignment container with `s` and `t` sequences,
    * representing `left` and `right` aligned sequences, respectively
    * @param sequences container with `left` and `right` original sequences
    * @param step container with current valid indexes
    * @return new step with `i` and `k` indexes modified
    */
  def consumeUp(alignments: Alignments, sequences: Sequences)(step: AlgorithmStep): F[AlgorithmStep] =
    (advanceLeft(alignments, sequences, step), skipRight(alignments, step)).parTupled.as(step.up)

  /**
    * Consuming 'UpLeft' in the Optimal Alignment Matrix means that choosing both sequences (match) best scores
    * instead of choosing either sequence only.
    *
    * @param alignment container with `s` and `t` sequences,
    * representing `left` and `right` aligned sequences, respectively
    * @param sequences container with `left` and `right` original sequences
    * @param step container with current valid indexes
    * @return new step with `i`, `j` and `k` indexes modified
    */
  def consumeUpLeft(alignments: Alignments, sequences: Sequences)(step: AlgorithmStep): F[AlgorithmStep] =
    (advanceLeft(alignments, sequences, step), advanceRight(alignments, sequences, step)).parTupled.as(step.upLeft)

  /**
    * Consuming 'Left' in the Optimal Alignment Matrix means that choosing the `right` sequence best scores
    * instead of aligning with `left` sequence.
    *
    * @param alignment container with `s` and `t` sequences,
    * representing `left` and `right` aligned sequences, respectively
    * @param sequences container with `left` and `right` original sequences
    * @param step container with current valid indexes
    * @return new step with `j` and `k` indexes modified
    */
  def consumeLeft(alignments: Alignments, sequences: Sequences)(step: AlgorithmStep): F[AlgorithmStep] =
    (skipLeft(alignments, step), advanceRight(alignments, sequences, step)).parTupled.as(step.left)

  // -----

  /**
    * Consumes `left` token at step's position `i`, setting into sequence `s` at `k`
    *
    * @param alignment container with `s` and `t` sequences,
    * representing `left` and `right` aligned sequences, respectively
    * @param sequences container with `left` and `right` original sequences
    * @param step container with current valid indexes
    */
  def advanceLeft(alignments: Alignments, sequences: Sequences, step: AlgorithmStep): F[Unit] = {
    import alignments._, sequences._, step._
    vector.set(s, k, left.at(i))
  }

  /**
    * Consumes `right` token at step's position `j`, setting into sequence `t` at `k`
    *
    * @param alignment container with `s` and `t` sequences,
    * representing `left` and `right` aligned sequences, respectively
    * @param sequences container with `left` and `right` original sequences
    * @param step container with current valid indexes
    */
  def advanceRight(alignments: Alignments, sequences: Sequences, step: AlgorithmStep): F[Unit] = {
    import alignments._, sequences._, step._
    vector.set(t, k, right.at(j))
  }

  /**
    * Skips `left` token at step's position `i`, setting `AlgorithmToken.Empty` on sequence `s` at `k`.
    *
    * @param alignment container with `s` and `t` sequences,
    * representing `left` and `right` aligned sequences, respectively
    * @param step container with current valid indexes
    */
  def skipLeft(alignments: Alignments, step: AlgorithmStep): F[Unit] = {
    import alignments._, step._, AlgorithmToken._
    vector.set(s, k, Empty)
  }

  /**
    * Skips `right` token at step's position `j`, setting `AlgorithmToken.Empty` on sequence `t` at `k`.
    *
    * @param alignment container with `s` and `t` sequences,
    * representing `left` and `right` aligned sequences, respectively
    * @param step container with current valid indexes
    */
  def skipRight(alignments: Alignments, step: AlgorithmStep): F[Unit] = {
    import alignments._, step._, AlgorithmToken._
    vector.set(t, k, Empty)
  }
}
