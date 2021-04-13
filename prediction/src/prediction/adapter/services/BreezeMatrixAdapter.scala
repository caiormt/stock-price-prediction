package prediction.adapter.services

import cats._
import cats.implicits._

import cats.effect._

import prediction.data.ports.services._

import breeze.linalg._
import breeze.math._

import scala.reflect._

final class BreezeMatrixAdapter[F[_]: Sync, Score: ClassTag: Semiring: Order]
    extends MatrixServicePort[F, DenseMatrix, Score] {

  override def empty(n: Int, m: Int): F[DenseMatrix[Score]] =
    Sync[F].delay(DenseMatrix.zeros[Score](n, m))

  override def set(matrix: DenseMatrix[Score], i: Int, j: Int, value: Score): F[Unit] =
    Sync[F].delay(matrix(i, j) = value)

  override def get(matrix: DenseMatrix[Score], i: Int, j: Int): F[Score] =
    Sync[F].delay(matrix(i, j))

  override def size(matrix: DenseMatrix[Score]): F[(Int, Int)]                       =
    Sync[F].delay(matrix.rows -> matrix.cols)

  override def maximumBy(matrix: DenseMatrix[Score], f: MatrixFilter): F[(Int, Int)] =
    Sync[F].delay(matrix.iterator.filter(f)).map(_.maxBy(_._2)).map(_._1)
}
