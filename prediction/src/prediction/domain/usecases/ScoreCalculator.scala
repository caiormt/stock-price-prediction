package prediction.domain.usecases

import prediction.domain.entities.algorithm._

object ScoreCalculator {
  def apply[F[_]](implicit ev: ScoreCalculator[F]): ScoreCalculator[F] = ev
}

trait ScoreCalculator[F[_]] {
  def calculate(first: AlgorithmToken, second: AlgorithmToken): F[AlgorithmScore]
}
