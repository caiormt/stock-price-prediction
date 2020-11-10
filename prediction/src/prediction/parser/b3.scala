package prediction.parser

import atto.Atto._
import atto._
import atto.syntax.refined._

import cats.implicits._

import prediction.domain.b3._

object b3 {
  val filename: Parser[Filename] =
    (string("COTAHIST."), count(4, digit))
      .mapN((prefix, digits) => s"$prefix${digits.mkString}")
      .refined[FileName]
      .map(Filename.apply)

  val sourceCode: Parser[String] =
    manyN(8, anyChar).map(_.mkString).map(_.trim)

  val totalRegisters: Parser[Long] =
    manyN(11, digit).map(_.mkString).map(_.toLong)

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
      _              <- skipManyN(2, anyChar)
      codNeg         <- stringTrim(12)
      _              <- skipManyN(3, anyChar)
      nomRes         <- stringTrim(12)
      especi         <- stringTrim(10)
      _              <- skipManyN(3, anyChar)
      modRef         <- stringTrim(4)
      preAbe         <- numericV11(13)
      preMax         <- numericV11(13)
      preMin         <- numericV11(13)
      preMed         <- numericV11(13)
      preUlt         <- numericV11(13)
      preOfc         <- numericV11(13)
      preOfv         <- numericV11(13)
      totNeg         <- numeric(5).map(_.toInt)
      quaTot         <- numeric(18).map(_.toInt)
      volTot         <- numericV11(18)
      _              <- skipManyN(57, anyChar)
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
