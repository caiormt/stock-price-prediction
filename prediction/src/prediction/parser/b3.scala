package prediction.parser

import atto.Atto._
import atto._
import atto.syntax.refined._

import cats.implicits._

import prediction.domain.b3._

import java.time.{ format => jtf }
import java.{ time => jt }

import scala.math.BigDecimal.RoundingMode

object b3 {
  val dateFormat: jtf.DateTimeFormatter =
    jtf.DateTimeFormatter.ofPattern("yyyyMMdd")

  def decimal(n: Int): Parser[BigDecimal] =
    numeric(n).map(BigDecimal.apply).map(_.setScale(2, RoundingMode.HALF_EVEN)).map(_ / 100)

  val date: Parser[jt.LocalDate] =
    count(8, digit).map(_.mkString).flatMap { str =>
      try ok(jt.LocalDate.parse(str, dateFormat))
      catch { case e: jtf.DateTimeParseException => err(e.toString()) }
    }

  val filename: Parser[Filename] =
    (string("COTAHIST."), integer(4))
      .mapN((prefix, digits) => s"$prefix$digits")
      .refined[FileName]
      .map(Filename.apply)

  val sourceCode: Parser[String] =
    take(8).map(_.trim)

  val totalRegisters: Parser[Long] =
    numeric(11).map(_.toLong)

  val header: Parser[Header] = {
    val header = (filename, sourceCode, date).mapN(Header.apply)
    string("00") ~> header <~ skipManyN(214, whitespace)
  }

  val trailer: Parser[Trailer] = {
    val trailer = (filename, sourceCode, date, totalRegisters).mapN(Trailer.apply)
    string("99") ~> trailer <~ skipManyN(203, whitespace)
  }

  val register: Parser[Register] =
    for {
      _              <- string("01")
      dateOfExchange <- date
      _              <- skipAny(2)
      codNeg         <- trim(12)
      _              <- skipAny(3)
      nomRes         <- trim(12)
      especi         <- trim(10)
      _              <- skipAny(3)
      modRef         <- trim(4)
      preAbe         <- decimal(13)
      preMax         <- decimal(13)
      preMin         <- decimal(13)
      preMed         <- decimal(13)
      preUlt         <- decimal(13)
      preOfc         <- decimal(13)
      preOfv         <- decimal(13)
      totNeg         <- integer(5)
      quaTot         <- integer(18)
      volTot         <- decimal(18)
      _              <- skipAny(57)
    } yield Register(
      dateOfExchange = dateOfExchange,
      codNeg = codNeg,
      nomRes = nomRes,
      especi = especi,
      modRef = modRef,
      preAbe = preAbe,
      preMax = preMax,
      preMin = preMin,
      preMed = preMed,
      preUlt = preUlt,
      preOfc = preOfc,
      preOfv = preOfv,
      totNeg = totNeg,
      quaTot = quaTot,
      volTot = volTot
    )
}
