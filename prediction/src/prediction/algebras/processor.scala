package prediction
package algebras

import cats._
import cats.implicits._

import cats.effect._
import cats.effect.concurrent._

import breeze.linalg._
import breeze.math._
import breeze.storage._

import scala.reflect._

trait Processor[F[_], Sequence, Token, Score] {
  def score(s: Token, t: Token): Score
  def matrix(): F[DenseMatrix[Score]]
  def similarity(): F[Score]
  def build(): F[Unit]
  def alignment(): F[(Sequence, Sequence)]
}

sealed abstract class NeedlemanWunsch[
    F[_]: Concurrent,
    Sequence: Monoid,
    Token: ClassTag: Zero,
    Score: ClassTag: Semiring: Eq
] extends Processor[F, Sequence, Token, Score] {

  import ext.Semiring._

  private val deferred: Deferred[F, DenseMatrix[Score]] =
    Deferred.unsafe[F, DenseMatrix[Score]]

  @inline def s(index: Int): Token
  @inline def t(index: Int): Token

  @inline def `|s|` : Int
  @inline def `|t|` : Int
  @inline def `'-'` : Token

  def prepare(M: DenseMatrix[Score]): F[Unit]
  def choose(a: Score, b: Score, c: Score): Score
  def token2Sequence(token: Token): Sequence

  @inline def startAlignment(M: DenseMatrix[Score]): (Int, Int)
  @inline def continueAlignment(V: Score, i: Int, j: Int): Boolean

  override def matrix(): F[DenseMatrix[Score]] =
    deferred.get.map(_.copy)

  override def build(): F[Unit] =
    for {
      M <- Sync[F].delay(DenseMatrix.zeros[Score](`|s|` + 1, `|t|` + 1))
      _ <- Sync[F].delay(M(0, 0) = ext.Semiring[Score].zero)
      _ <- prepare(M)
      _ <- Sync[F].delay {
             for (i <- 1 to `|s|`)
               for (j <- 1 to `|t|`) {
                 // format: off
                 val a   = M(i    , j - 1) + score(`'-'`, t(j) )
                 val b   = M(i - 1, j - 1) + score(s(i) , t(j) )
                 val c   = M(i - 1, j    ) + score(s(i) , `'-'`)
                 M(i, j) = choose(a, b, c)
                 // format: on
               }
           }.void
      _ <- deferred.complete(M)
    } yield ()

  override def alignment(): F[(Sequence, Sequence)] =
    for {
      M    <- deferred.get
      size <- Sync[F].delay(Math.max(`|s|`, `|t|`))
      `~s` <- Sync[F].delay(DenseVector.zeros[Token](size * 2))
      `~t` <- Sync[F].delay(DenseVector.zeros[Token](size * 2))
      st   <- Sync[F].delay {
                // Indexes
                var (i, j) = `|s|` -> `|t|`
                var k      = 0

                locally {
                  val (_i, _j) = startAlignment(M)
                  for (_ <- _i until `|s|`) up()
                  for (_ <- _j until `|t|`) left()
                }

                def diag(): Unit = {
                  `~s`(k) = s(i)
                  `~t`(k) = t(j)
                  i = i - 1; j = j - 1; k = k + 1
                }

                def left(): Unit = {
                  `~s`(k) = `'-'`
                  `~t`(k) = t(j)
                  j = j - 1; k = k + 1
                }

                def up(): Unit = {
                  `~s`(k) = s(i)
                  `~t`(k) = `'-'`
                  i = i - 1; k = k + 1
                }

                // Build the alignment
                while (continueAlignment(M(i, j), i, j))
                  // format: off
                  if      (M(i, j) === M(i - 1, j - 1) + score(s(i) , t(j)) ) diag()
                  else if (M(i, j) === M(i    , j - 1) + score(`'-'`, t(j)) ) left()
                  else if (M(i, j) === M(i - 1, j    ) + score(s(i) , `'-'`)) up()
                  // format: on

                // Complete the remaining sequences
                while (i =!= 0) up()
                while (j =!= 0) left()

                val _s = `~s`.toScalaVector().reverse.foldMap(token2Sequence)
                val _t = `~t`.toScalaVector().reverse.foldMap(token2Sequence)
                (_s, _t)
              }
    } yield st
}

// ----- Global Processor -----

object GlobalProcessor {
  def make[F[_]: Concurrent](s: String, t: String): F[Processor[F, String, Char, Long]] =
    Sync[F].delay(new GlobalProcessor[F](s, t))
}

final class GlobalProcessor[F[_]: Concurrent] private (s: String, t: String)
    extends NeedlemanWunsch[F, String, Char, Long] {

  override def s(index: Int): Char = s.charAt(index - 1)
  override def t(index: Int): Char = t.charAt(index - 1)

  override def `|s|` : Int  = s.size
  override def `|t|` : Int  = t.size
  override def `'-'` : Char = '-'

  override def startAlignment(M: DenseMatrix[Long]): (Int, Int) =
    `|s|` -> `|t|`

  override def continueAlignment(V: Long, i: Int, j: Int): Boolean =
    i =!= 0 && j =!= 0

  override def score(s: Char, t: Char): Long =
    (s, t) match {
      case (_, '-')          => -2L
      case ('-', _)          => -2L
      case (s, t) if s =!= t => -1L
      case (s, t) if s === t => +1L
    }

  override def similarity(): F[Long] =
    matrix().map(M => M(`|s|`, `|t|`))

  override def prepare(M: DenseMatrix[Long]): F[Unit] =
    for {
      _ <- Sync[F].delay {
             for (k <- 1 to `|t|`)
               M(0, k) = k * score(`'-'`, t(k))
           }
      _ <- Sync[F].delay {
             for (k <- 1 to `|s|`)
               M(k, 0) = k * score(s(k), `'-'`)
           }
    } yield ()

  override def choose(a: Long, b: Long, c: Long): Long =
    Set(a, b, c).max

  override def token2Sequence(token: Char): String =
    token.toString
}

// ----- Semi Global Processor -----

object SemiGlobalProcessor {
  def make[F[_]: Concurrent](s: String, t: String): F[Processor[F, String, Char, Long]] =
    Sync[F].delay(new SemiGlobalProcessor[F](s, t))
}

final class SemiGlobalProcessor[F[_]: Concurrent] private (s: String, t: String)
    extends NeedlemanWunsch[F, String, Char, Long] {

  override def s(index: Int): Char = s.charAt(index - 1)
  override def t(index: Int): Char = t.charAt(index - 1)

  override def `|s|` : Int  = s.size
  override def `|t|` : Int  = t.size
  override def `'-'` : Char = '-'

  implicit val ordering: Ordering[((Int, Int), Long)] =
    Ordering.by(_._2)

  override def startAlignment(M: DenseMatrix[Long]): (Int, Int)    =
    M.iterator.filter { case ((_, j), _) => j === `|t|` }.max._1

  override def continueAlignment(V: Long, i: Int, j: Int): Boolean =
    i =!= 0 && j =!= 0

  override def score(s: Char, t: Char): Long =
    (s, t) match {
      case (_, '-')          => -2L
      case ('-', _)          => -2L
      case (s, t) if s =!= t => -1L
      case (s, t) if s === t => +1L
    }

  override def similarity(): F[Long] =
    matrix().map(M => Math.max(max(M(::, -1)), max(M(-1, ::))))

  override def prepare(M: DenseMatrix[Long]): F[Unit] =
    Sync[F].delay {
      for (k <- 1 to `|t|`)
        M(0, k) = k * score(`'-'`, t(k))
    }

  override def choose(a: Long, b: Long, c: Long): Long =
    Set(a, b, c).max

  override def token2Sequence(token: Char): String =
    token.toString
}

// ----- Local Processor -----

object LocalProcessor {
  def make[F[_]: Concurrent](s: String, t: String): F[Processor[F, String, Char, Long]] =
    Sync[F].delay(new LocalProcessor[F](s, t))
}

final class LocalProcessor[F[_]: Concurrent] private (s: String, t: String)
    extends NeedlemanWunsch[F, String, Char, Long] {

  override def s(index: Int): Char = s.charAt(index - 1)
  override def t(index: Int): Char = t.charAt(index - 1)

  override def `|s|` : Int  = s.size
  override def `|t|` : Int  = t.size
  override def `'-'` : Char = '-'

  implicit val ordering: Ordering[((Int, Int), Long)] =
    Ordering.by(_._2)

  override def startAlignment(M: DenseMatrix[Long]): (Int, Int) =
    M.iterator.max._1

  override def continueAlignment(V: Long, i: Int, j: Int): Boolean =
    V =!= 0

  override def score(s: Char, t: Char): Long =
    (s, t) match {
      case (_, '-')          => -2L
      case ('-', _)          => -2L
      case (s, t) if s =!= t => -1L
      case (s, t) if s === t => +1L
    }

  override def similarity(): F[Long] =
    matrix().map(M => max(M))

  override def prepare(M: DenseMatrix[Long]): F[Unit] =
    Applicative[F].unit

  override def choose(a: Long, b: Long, c: Long): Long =
    Set(a, b, c, 0).max

  override def token2Sequence(token: Char): String =
    token.toString
}
