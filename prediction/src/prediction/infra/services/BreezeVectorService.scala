package prediction.infra.services

import cats.effect._

import prediction.data.ports.services._
import prediction.domain.entities.algorithm._

import breeze.linalg._

object BreezeVectorService {
  def make[F[_]: Sync]: F[BreezeVectorService[F]] =
    Sync[F].delay(new BreezeVectorService[F])
}

final class BreezeVectorService[F[_]: Sync] private () extends VectorService[F, DenseVector] {
  override def empty(n: Int): F[DenseVector[AlgorithmToken]] =
    Sync[F].delay(DenseVector.zeros[AlgorithmToken](n))

  override def set(vector: DenseVector[AlgorithmToken], n: Int, value: AlgorithmToken): F[Unit] =
    Sync[F].delay(vector(n) = value)

  override def toVector(vector: DenseVector[AlgorithmToken]): F[scala.Vector[AlgorithmToken]] =
    Sync[F].delay(vector.toScalaVector())
}
