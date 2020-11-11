package prediction.algebras

import breeze.linalg._
// import breeze.numerics._

// import cats._
import cats.effect._
import cats.implicits._

trait Processor[F[_]] {
  def score(left: Char, right: Char): Long
  def build(left: String, right: String): DenseMatrix[Long]
  def process(sequence: String): F[Unit]
}

object LiveProcessor {
  def make[F[_]: Sync]: F[Processor[F]] =
    Sync[F].delay(new LiveProcessor[F])
}

final class LiveProcessor[F[_]] extends Processor[F] {

  override def score(left: Char, right: Char): Long =
    (left, right) match {
      case (_, '-')          => -2
      case ('-', _)          => -2
      case (a, b) if a =!= b => -1
      case (a, b) if a === b => 1
    }

  override def build(left: String, right: String): DenseMatrix[Long] = {
    val `|s|` = left.length()
    val `|t|` = right.length()
    val M     = DenseMatrix.zeros[Long](`|s|`, `|t|`)

    // First position filled with zero.
    M(0, 0) = 0L

    // Values associated with column alignment
    for (k <- 0 until `|t|`)
      M(0, k) = k * score('-', right.charAt(k))

    // Values associated with row alignment
    for (k <- 0 until `|s|`)
      M(k, 0) = k * score(left.charAt(k), '-')

    for (i <- 0 until `|t|`)
      for (j <- 0 until `|s|`) {
        val l1 = M(i, j - 1) + score('-', right.charAt(j))
        val l2 = M(i - 1, j - 1) + score(left.charAt(i), right.charAt(j))
        val l3 = M(i - 1, j) + score(left(i), '-')
        M(i, j) = Math.max(Math.max(l1, l2), l3)
      }

    M
  }

  override def process(sequence: String): F[Unit] = ???
}
