package prediction.data.parsers

import atto.Atto._
import atto._

import cats.implicits._

import prediction.data.models.b3._

import java.time.{ format => jtf }
import java.{ time => jt }

import scala.math.BigDecimal._

object b3 {
  val dateFormat: jtf.DateTimeFormatter =
    jtf.DateTimeFormatter.ofPattern("yyyyMMdd")

  def skipAny(n: Int): Parser[Unit] =
    skipManyN(n, anyChar)

  def numeric(n: Int): Parser[BigInt] =
    manyN(n, digit).map(_.mkString).map(BigInt.apply)

  def integer(n: Int): Parser[Int] =
    numeric(n).map(_.toInt)

  def decimal(n: Int): Parser[BigDecimal] =
    numeric(n).map(BigDecimal.apply).map(_.setScale(2, RoundingMode.HALF_EVEN)).map(_ / 100)

  def trim(n: Int): Parser[String] =
    take(n).map(_.trim)

  // -----

  val filename: Parser[String] =
    (string("COTAHIST."), integer(4))
      .mapN((prefix, digits) => s"$prefix$digits")

  val sourceCode: Parser[String] =
    trim(8)

  val date: Parser[jt.LocalDate] =
    count(8, digit).map(_.mkString).flatMap { str =>
      try ok(jt.LocalDate.parse(str, dateFormat))
      catch { case e: jtf.DateTimeParseException => err(e.toString()) }
    }

  val totalRegisters: Parser[Long] =
    numeric(11).map(_.toLong)

  // -----

  val header: Parser[Header] = {
    val header = (filename, sourceCode, date).mapN(Header.apply)
    string("00") ~> header <~ skipManyN(214, whitespace)
  }

  val trailer: Parser[Trailer] = {
    val trailer = (filename, sourceCode, date, totalRegisters).mapN(Trailer.apply)
    string("99") ~> trailer <~ skipManyN(203, whitespace)
  }

  val register: Parser[Register] = {
    // format: off
    val dateOfExchange = date     <~ skipAny(2)
    val codNeg         = trim(12) <~ skipAny(3)
    val nomRes         = trim(12)
    val especi         = trim(10) <~ skipAny(3)
    val modRef         = trim(4)
    val preAbe         = decimal(13)
    val preMax         = decimal(13)
    val preMin         = decimal(13)
    val preMed         = decimal(13)
    val preUlt         = decimal(13)
    val preOfc         = decimal(13)
    val preOfv         = decimal(13)
    val totNeg         = integer(5)
    val quaTot         = integer(18)
    val volTot         = decimal(18)
    // format: on

    val register = (
      dateOfExchange,
      codNeg,
      nomRes,
      especi,
      modRef,
      preAbe,
      preMax,
      preMin,
      preMed,
      preUlt,
      preOfc,
      preOfv,
      totNeg,
      quaTot,
      volTot
    ).mapN(Register.apply)

    string("01") ~> register <~ skipAny(57)
  }

  val entry: Parser[Entry] =
    choice(header, register, trailer)
}
