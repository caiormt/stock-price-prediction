package prediction.algebras

import breeze.linalg._

import cats.effect._
import cats.implicits._
import cats.effect.concurrent._

trait Processor[F[_], Token, Score] {
  def score(s: Token, t: Token): Score
  def build(): F[Unit]
  def getMatrix(): F[DenseMatrix[Score]]
}

object LiveProcessor {
  def make[F[_]: Concurrent](s: String, t: String): F[Processor[F, Char, Long]] =
    Deferred[F, DenseMatrix[Long]].flatMap { deferred =>
      Sync[F].delay(new LiveProcessor[F](s, t, deferred))
    }
}

final class LiveProcessor[F[_]: Sync] private (s: String, t: String, deferred: Deferred[F, DenseMatrix[Long]])
    extends Processor[F, Char, Long] {

  override def score(s: Char, t: Char): Long =
    (s, t) match {
      case (_, '-')          => -2
      case ('-', _)          => -2
      case (s, t) if s =!= t => -1
      case (s, t) if s === t => 1
    }

  override def build(): F[Unit] =
    Sync[F]
      .delay {
        // Calculate sizes
        val `|s|` = s.length()
        val `|t|` = t.length()

        // Create matrix
        val M = DenseMatrix.zeros[Long](`|s|`, `|t|`)

        // First position filled with zero.
        M(0, 0) = 0L

        // Values associated with column alignment
        for (k <- 0 until `|t|`)
          M(0, k) = k * score('-', t(k))

        // Values associated with row alignment
        for (k <- 0 until `|s|`)
          M(k, 0) = k * score(s(k), '-')

        // Run the algorithm
        for (i <- 0 until `|t|`)
          for (j <- 0 until `|s|`) {
            // format: off
            val l1 = M(i    , j - 1) + score('-' , t(j))
            val l2 = M(i - 1, j - 1) + score(s(i), t(j))
            val l3 = M(i - 1, j)     + score(s(i), '-')
            M(i, j) = Math.max(Math.max(l1, l2), l3)
            // format: on
          }

        M
      }
      .flatMap(deferred.complete)

  override def getMatrix(): F[DenseMatrix[Long]] =
    deferred.get.flatMap { matrix =>
      Sync[F].delay(matrix.copy)
    }
}
