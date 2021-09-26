package prediction.adapter.services

import cats.implicits._

import cats.effect._

import prediction.data.ports.services._

import breeze.linalg._
import breeze.math._
import natchez.TraceValue._
import natchez._

import scala.reflect._

final class BreezeVectorAdapter[F[_]: Sync: Trace, Token: ClassTag: Semiring]
    extends VectorServicePort[F, DenseVector, Token] {

  override def empty(n: Int): F[DenseVector[Token]] =
    Trace[F].put(("vector-rows", n)) *> Trace[F].span("vector-create") {
      Sync[F].delay(DenseVector.zeros(n))
    }

  override def set(vector: DenseVector[Token], i: Int, value: Token): F[Unit] =
    Sync[F].delay(vector(i) = value)

  override def get(vector: DenseVector[Token], i: Int): F[Token] =
    Sync[F].delay(vector(i))

  override def toScalaVector(vector: DenseVector[Token]): F[scala.collection.immutable.Vector[Token]] =
    Sync[F].delay(vector.toScalaVector())
}
