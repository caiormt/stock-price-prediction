package prediction.data.services

import cats._

import cats.effect._

import munit._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._

import java.{ time => jt }

final class AlgorithmParserServiceSpec extends CatsEffectSuite {

  private val service = Eval.later(new AlgorithmParserService3[IO])

  test("should return Draw when opening and closing price are equals") {
    val quotation = Quotation(
      QuotationExchangeDate(jt.LocalDateTime.now),
      QuotationNegotiationCode("BOVA11"),
      QuotationOpeningPrice(5),
      QuotationClosingPrice(5)
    )

    val result = service.value.parse(quotation)

    assertIO(result, AlgorithmToken(Alphabet.Draw))
  }

  test("should return Negative when closing is lesser than opening price") {
    val quotation = Quotation(
      QuotationExchangeDate(jt.LocalDateTime.now),
      QuotationNegotiationCode("BOVA11"),
      QuotationOpeningPrice(10),
      QuotationClosingPrice(5)
    )

    val result = service.value.parse(quotation)

    assertIO(result, AlgorithmToken(Alphabet.Negative1))
  }

  test("should return Positive when opening is lesser than closing price") {
    val quotation = Quotation(
      QuotationExchangeDate(jt.LocalDateTime.now),
      QuotationNegotiationCode("BOVA11"),
      QuotationOpeningPrice(5),
      QuotationClosingPrice(10)
    )

    val result = service.value.parse(quotation)

    assertIO(result, AlgorithmToken(Alphabet.Positive1))
  }
}
