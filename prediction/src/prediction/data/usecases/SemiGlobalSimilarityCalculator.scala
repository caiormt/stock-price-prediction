package prediction.data.usecases

import cats._
import cats.implicits._

import cats.effect._

import prediction.data.ports._
import prediction.domain.entities.algorithm._
import prediction.domain.usecases._

object SemiGlobalSimilarityCalculator {
  def make[F[_]: Sync: Parallel, M[_]](matrix: MatrixService[F, M]): F[SimilarityCalculator[F, M]] =
    Sync[F].delay(new SemiGlobalSimilarityCalculator[F, M](matrix))
}

final class SemiGlobalSimilarityCalculator[F[_]: FlatMap: Parallel, M[_]] private (matrix: MatrixService[F, M])
    extends SimilarityCalculator[F, M] {

  override def calculate(M: M[AlgorithmScore]): F[AlgorithmScore] =
    (matrix.rows(M), matrix.cols(M)).parMapN {
      case (rows, cols) => matrix.max(M, rows - 1, cols - 1)
    }.flatten
}
