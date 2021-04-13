package prediction.adapter.repositories.parsers

import atto._

import cats.implicits._

import munit._

import prediction.adapter.repositories.models.b3._
import prediction.adapter.repositories.parsers.b3._

import java.{ time => jt }

final class B3ParserSpec extends CatsEffectSuite {

  test("should parse a header correctly") {
    val row =
      "00COTAHIST.2019BOVESPA 20191230                                                                                                                                                                                                                      "

    val expected =
      Header(
        fileName = "COTAHIST.2019",
        sourceCode = "BOVESPA",
        fileGenerationDate = jt.LocalDate.of(2019, 12, 30)
      )

    val result = Parser.parse(header, row).option

    assertEquals(result, expected.some)
  }

  test("should parse an entry correctly") {
    val row =
      "012019010202BERK34      010BERKSHIRE   DRN          R$  000000007827500000000782750000000077348000000007756300000000773480000000076181000000007772100003000000000000000500000000000038781900000000000000009999123100000010000000000000BRBERKBDR002100"

    val expected =
      Register(
        dateOfExchange = jt.LocalDate.of(2019, 1, 2),
        codNeg = "BERK34",
        nomRes = "BERKSHIRE",
        especi = "DRN",
        modRef = "R$",
        preAbe = 782.75,
        preMax = 782.75,
        preMin = 773.48,
        preMed = 775.63,
        preUlt = 773.48,
        preOfc = 761.81,
        preOfv = 777.21,
        totNeg = 3,
        quaTot = 500,
        volTot = 387819.00
      )

    val result = Parser.parse(entry, row).option

    assertEquals(result, expected.some)
  }

  test("should parse a tail correctly") {
    val row =
      "99COTAHIST.2019BOVESPA 2019123000000779231                                                                                                                                                                                                           "

    val expected =
      Trailer(
        fileName = "COTAHIST.2019",
        sourceCode = "BOVESPA",
        fileGenerationDate = jt.LocalDate.of(2019, 12, 30),
        totalRegisters = 779231
      )

    val result = Parser.parse(trailer, row).option

    assertEquals(result, expected.some)
  }
}
