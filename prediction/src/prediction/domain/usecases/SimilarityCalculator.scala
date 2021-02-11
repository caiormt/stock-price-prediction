package prediction.domain.usecases

import prediction.domain.entities.algorithm._

object SimilarityCalculator {
  def apply[F[_], C[_]](implicit ev: SimilarityCalculator[F, C]): SimilarityCalculator[F, C] = ev
}

trait SimilarityCalculator[F[_], C[_]] {
  def calculate(matrix: C[AlgorithmScore]): F[AlgorithmScore]
}
