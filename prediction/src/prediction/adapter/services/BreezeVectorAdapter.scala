package prediction.adapter.services

import cats.effect._

import prediction.data.ports.services._

import breeze.linalg._
import breeze.math._

import scala.reflect._

final class BreezeVectorAdapter[F[_]: Sync, Token: ClassTag: Semiring]
    extends VectorServicePort[F, DenseVector, Token] {

  override def empty(n: Int): F[DenseVector[Token]] =
    Sync[F].delay(DenseVector.zeros(n))

  override def set(vector: DenseVector[Token], i: Int, value: Token): F[Unit] =
    Sync[F].delay(vector(i) = value)

  override def get(vector: DenseVector[Token], i: Int): F[Token] =
    Sync[F].delay(vector(i))

  override def toScalaVector(vector: DenseVector[Token]): F[scala.collection.immutable.Vector[Token]] =
    Sync[F].delay(vector.toScalaVector())
}
