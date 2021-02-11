package prediction.data.usecases

import cats._
import cats.data._
import cats.implicits._

import cats.effect._

import prediction.data.ports._
import prediction.domain.entities.algorithm._
import prediction.domain.usecases._

object SemiGlobalOptimalAlignment {
  def make[F[_]: Sync: Parallel, M[_], V[_]](
      matrix: MatrixService[F, M],
      vector: VectorService[F, V],
      score: ScoreCalculator[F]
  ): F[OptimalAlignment[F, M]] =
    Sync[F].delay(new SemiGlobalOptimalAlignment[F, M, V](matrix, vector, score))

  // -----

  final private class Step(val i: Int, val j: Int, val k: Int)
  implicit final private class StepOps(private val step: Step) extends AnyVal {
    import step._
    // format: off
    def consumeUp: Step     = new Step(i - 1, j    , k + 1)
    def consumeUpLeft: Step = new Step(i - 1, j - 1, k + 1)
    def consumeLeft: Step   = new Step(i    , j - 1, k + 1)
    // format: on
  }
}

final class SemiGlobalOptimalAlignment[F[_]: Sync: Parallel, M[_], V[_]] private (
    matrix: MatrixService[F, M],
    vector: VectorService[F, V],
    score: ScoreCalculator[F]
) extends OptimalAlignment[F, M] {

  import AlgorithmToken._
  import SemiGlobalOptimalAlignment._

  override def align(
      first: AlgorithmSequence,
      second: AlgorithmSequence,
      M: M[AlgorithmScore]
  ): F[AlgorithmAlignment] = {
    def calculateBestAlignment(S: V[AlgorithmToken], T: V[AlgorithmToken]): F[Unit] = {

      def consumeUp(S: V[AlgorithmToken], T: V[AlgorithmToken], step: Step): F[Step] = {
        import step._
        (vector.set(S, k, first(i)), vector.set(T, k, Empty)).parTupled
          .as(step.consumeUp)
      }

      def consumeUpLeft(S: V[AlgorithmToken], T: V[AlgorithmToken], step: Step): F[Step] = {
        import step._
        (vector.set(S, k, first(i)), vector.set(T, k, second(j))).parTupled
          .as(step.consumeUpLeft)
      }

      def consumeLeft(S: V[AlgorithmToken], T: V[AlgorithmToken], step: Step): F[Step] = {
        import step._
        (vector.set(S, k, Empty), vector.set(T, k, second(j))).parTupled
          .as(step.consumeLeft)
      }

      // -----

      def initializeForFirstSequenceEmptyTokenPenalty(M: M[AlgorithmScore]): F[Step] = {
        def consume(row: Int, col: Int, i: Int): F[Int] = {
          def go(step: Step): F[Int] =
            if (step.i === i) Applicative[F].pure(step.k)
            else consumeUp(S, T, step) >>= go

          go(new Step(row, col, 0))
        }

        for {
          rows   <- matrix.rows(M)
          cols   <- matrix.cols(M)
          (i, j) <- matrix.coordinatesMaxColumn(M, cols - 1)
          k      <- consume(rows - 1, cols - 1, i)
        } yield new Step(i, j, k)
      }

      // -----

      def go(step: Step): F[Step] = {
        import step._

        def calculate: F[Step] = {
          def sum(f: F[AlgorithmScore], g: F[AlgorithmScore]): F[AlgorithmScore] =
            (f, g).parMapN { case (f, g) => f + g }

          // format: off
          val up     = sum(matrix.get(M, i - 1, j    ), score.calculate(first(i), Empty    ))
          val upLeft = sum(matrix.get(M, i - 1, j - 1), score.calculate(first(i), second(j)))
          val left   = sum(matrix.get(M, i    , j - 1), score.calculate(Empty   , second(j)))

          // format: on
          val value = matrix.get(M, i, j)

          // format: off
          (value, up, upLeft, left).parMapN {
            case (best, _, value, _) if best === value => consumeUpLeft(S, T, step) >>= go
            case (best, _, _, value) if best === value => consumeLeft(  S, T, step) >>= go
            case (best, value, _, _) if best === value => consumeUp(    S, T, step) >>= go
          }.flatten
          // format: on
        }

        // format: off
        (i, j) match {
          case (0, 0) => Sync[F].delay(step)
          case (_, 0) => consumeUp(  S, T, step) >>= go
          case (0, _) => consumeLeft(S, T, step) >>= go
          case _      => calculate
        }
        // format: on
      }

      for {
        step <- initializeForFirstSequenceEmptyTokenPenalty(M)
        _    <- go(step)
      } yield ()
    }

    def buildAlignment(S: V[AlgorithmToken], T: V[AlgorithmToken]): F[AlgorithmAlignment] = {
      def reverseApply(V: V[AlgorithmToken]): F[AlgorithmSequence] =
        vector.toVector(V).map(_.reverse).map(AlgorithmSequence.apply)

      (reverseApply(S), reverseApply(T))
        .parMapN(AlgorithmAlignment.apply)
    }

    Sync[F].uncancelable {
      for {
        size      <- Sync[F].delay(first.value.size * second.value.size)
        S         <- vector.empty(size)
        T         <- vector.empty(size)
        _         <- calculateBestAlignment(S, T)
        alignment <- buildAlignment(S, T)
      } yield alignment
    }
  }

  override def buildMatrix(first: AlgorithmSequence, second: AlgorithmSequence): F[M[AlgorithmScore]] = {
    def initializeForFirstSequenceEmptyTokenPenalty(M: M[AlgorithmScore]): F[Unit] =
      (1 to second.value.size).toList.traverse_ { k =>
        for {
          s <- score.calculate(Empty, second(k))
          _ <- matrix.set(M, 0, k, s * k)
        } yield ()
      }

    def calculateBestAlignmentMatrix(M: M[AlgorithmScore]): F[Unit] =
      (1 to first.value.size).toList.traverse_ { i =>
        (1 to second.value.size).toList.traverse_ { j =>
          def sum(f: F[AlgorithmScore], g: F[AlgorithmScore]): F[AlgorithmScore] =
            (f, g).parMapN { case (f, g) => f + g }

          // format: off
          val up     = sum(matrix.get(M, i - 1, j    ), score.calculate(first(i), Empty    ))
          val upLeft = sum(matrix.get(M, i - 1, j - 1), score.calculate(first(i), second(j)))
          val left   = sum(matrix.get(M, i    , j - 1), score.calculate(Empty   , second(j)))
          // format: on

          NonEmptyList.of(up, upLeft, left).parSequence.map(_.maximum) >>= (matrix.set(M, i, j, _))
        }
      }

    Sync[F].uncancelable {
      for {
        M <- matrix.empty(first.value.size + 1, second.value.size + 1)
        _ <- matrix.set(M, 0, 0, AlgorithmScore.Empty)
        _ <- initializeForFirstSequenceEmptyTokenPenalty(M)
        _ <- calculateBestAlignmentMatrix(M)
      } yield M
    }
  }
}
