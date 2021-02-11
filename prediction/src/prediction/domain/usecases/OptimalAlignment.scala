package prediction.domain.usecases

import prediction.domain.entities.algorithm._

object OptimalAlignment {
  def apply[F[_], M[_]](implicit ev: OptimalAlignment[F, M]): OptimalAlignment[F, M] = ev
}

trait OptimalAlignment[F[_], M[_]] {
  def align(first: AlgorithmSequence, second: AlgorithmSequence, M: M[AlgorithmScore]): F[AlgorithmAlignment]
  def buildMatrix(first: AlgorithmSequence, second: AlgorithmSequence): F[M[AlgorithmScore]]
}
