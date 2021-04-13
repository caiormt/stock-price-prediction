package prediction.data.services

import cats._
import cats.data._
import cats.implicits._

import prediction.domain.entities.algorithm._
import prediction.domain.usecases._

import prediction.data.ports.services._

final class OptimalAlignmentBuilderService[F[_]: Monad: Parallel, Matrix[_]](
    matrix: MatrixServicePort[F, Matrix, AlgorithmScore],
    scoreCalculator: ScoreCalculatorUseCase[F, AlgorithmToken, AlgorithmScore]
) extends OptimalAlignmentBuilderUseCase[F, AlgorithmSequence, Matrix, AlgorithmScore] {

  override def build(left: AlgorithmSequence, right: AlgorithmSequence): F[Matrix[AlgorithmScore]] =
    for {
      (rows, columns) <- Applicative[F].pure(left.size -> right.size)
      m               <- matrix.empty(rows + 1, columns + 1)
      _               <- initFirstRowPenalty(right)(m)
      _               <- fillMatrix(left, right)(m)
    } yield m

  /**
    * Initialize all columns on first row applying penalty.
    *
    * This implies that GAPs on start of left sequence are penalized.
    *
    * @param right AlgorithmSequence used to add penalty.
    * @param M Matrix of Optimal Alignments
    */
  def initFirstRowPenalty(right: AlgorithmSequence)(m: Matrix[AlgorithmScore]): F[Unit] =
    (1 to right.size).toList.parTraverse_ { k =>
      import AlgorithmToken._
      for {
        s <- scoreCalculator.calculate(Empty, right.at(k))
        _ <- matrix.set(m, 0, k, s * k)
      } yield ()
    }

  /**
    * Build the Optimal Alignment matrix, choosing alignment that best scores between smaller prefixes already stored.
    *
    * @param left AlgorithmSequence
    * @param right AlgorithmSequence
    * @param M Matrix of Optimal Alignments
    */
  def fillMatrix(left: AlgorithmSequence, right: AlgorithmSequence)(m: Matrix[AlgorithmScore]): F[Unit] =
    (1 to left.size).toList.traverse_ { i =>
      (1 to right.size).toList.traverse_ { j =>
        import AlgorithmToken._

        // format: off
        val UP      = sum(matrix.get(m, i - 1, j    ), scoreCalculator.calculate(left.at(i), Empty      ))
        val UP_LEFT = sum(matrix.get(m, i - 1, j - 1), scoreCalculator.calculate(left.at(i), right.at(j)))
        val LEFT    = sum(matrix.get(m, i    , j - 1), scoreCalculator.calculate(Empty     , right.at(j)))
        // format: on

        NonEmptyList.of(UP, UP_LEFT, LEFT).parSequence.map(_.maximum) >>= (matrix.set(m, i, j, _))
      }
    }

  private def sum(f: F[AlgorithmScore], g: F[AlgorithmScore]): F[AlgorithmScore] =
    (f, g).parMapN { case (f, g) => f + g }
}
