package prediction.adapter.repositories.models

import munit._

import prediction.domain.entities.quotation._

import prediction.adapter.repositories.models.b3._

import java.{ time => jt }

final class B3ModelSpec extends CatsEffectSuite {

  test("should ignore header") {
    val header =
      Header(
        fileName = "COTAHIST.2019",
        sourceCode = "BOVESPA",
        fileGenerationDate = jt.LocalDate.of(2019, 12, 30)
      )

    assertEquals(collector.isDefinedAt(header), false)
  }

  test("should ignore trailer") {
    val trailer =
      Trailer(
        fileName = "COTAHIST.2019",
        sourceCode = "BOVESPA",
        fileGenerationDate = jt.LocalDate.of(2019, 12, 30),
        totalRegisters = 779231
      )

    assertEquals(collector.isDefinedAt(trailer), false)
  }

  test("should map register") {
    val register =
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

    val expected =
      Quotation(
        QuotationExchangeDate(jt.LocalDate.of(2019, 1, 2).atStartOfDay()),
        QuotationNegotiationCode("BERK34"),
        QuotationOpeningPrice(782.75),
        QuotationClosingPrice(773.48)
      )

    assertEquals(collector.isDefinedAt(register), true)
    assertEquals(collector(register), expected)
  }
}
