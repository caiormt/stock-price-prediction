package prediction.infra.services

import cats.data._
import cats.implicits._

import cats.effect._

import prediction.data.ports.services._
import prediction.domain.entities.algorithm._

import breeze.linalg.{ max => bmax, _ }

object BreezeMatrixService {
  def make[F[_]: Sync]: F[BreezeMatrixService[F]] =
    Sync[F].delay(new BreezeMatrixService[F])
}

final class BreezeMatrixService[F[_]: Sync] private () extends MatrixService[F, DenseMatrix] {
  override def empty(n: Int, m: Int): F[DenseMatrix[AlgorithmScore]] =
    Sync[F].delay(DenseMatrix.zeros[AlgorithmScore](n, m))

  override def get(matrix: DenseMatrix[AlgorithmScore], i: RowIndex, j: ColumnIndex): F[AlgorithmScore] =
    Sync[F].delay(matrix(i.value, j.value))

  override def set(matrix: DenseMatrix[AlgorithmScore], i: RowIndex, j: ColumnIndex, value: AlgorithmScore): F[Unit] =
    Sync[F].delay(matrix(i.value, j.value) = value)

  override def rows(matrix: DenseMatrix[AlgorithmScore]): F[Int] =
    Sync[F].delay(matrix.rows)

  override def cols(matrix: DenseMatrix[AlgorithmScore]): F[Int] =
    Sync[F].delay(matrix.cols)

  override def max(matrix: DenseMatrix[AlgorithmScore], i: RowIndex, j: ColumnIndex): F[AlgorithmScore] =
    Sync[F].delay(NonEmptySet.of(bmax(matrix(i.value, ::)), bmax(matrix(::, j.value))).maximum)

  override def coordinatesMaxColumn(matrix: DenseMatrix[AlgorithmScore], j: ColumnIndex): F[(RowIndex, ColumnIndex)] =
    Sync[F].delay(matrix(::, j.value).iterator.maxBy(_._2)._1).map(RowIndex.apply).tupleRight(j)
}
