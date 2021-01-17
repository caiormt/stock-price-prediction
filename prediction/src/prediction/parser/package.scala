package prediction

import atto.Atto._
import atto._

package object parser {
  def skipAny(n: Int): Parser[Unit] =
    skipManyN(n, anyChar)

  def numeric(n: Int): Parser[BigInt] =
    manyN(n, digit).map(_.mkString).map(BigInt.apply)

  def integer(n: Int): Parser[Int] =
    numeric(n).map(_.toInt)

  def trim(n: Int): Parser[String] =
    take(n).map(_.trim)
}
