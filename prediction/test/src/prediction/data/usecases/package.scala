package prediction.data

import cats.implicits._

import prediction.domain.entities._
import prediction.domain.entities.quotation._

import org.scalacheck._

import java.{ time => jt }

package object usecases {
  val nonEmptyAlphabetGen: Gen[Alphabet] =
    Gen.oneOf(Alphabet.Positive, Alphabet.Draw, Alphabet.Negative)

  val nonEmptyTupledAlphabetGen: Gen[(Alphabet, Alphabet)] =
    for {
      a <- nonEmptyAlphabetGen
      b <- nonEmptyAlphabetGen.suchThat(_ =!= a)
    } yield a -> b

  // -----

  val quotationExchangeDateGen: Gen[QuotationExchangeDate] =
    Gen.calendar
      .map(c => jt.LocalDateTime.ofInstant(c.toInstant, jt.ZoneId.systemDefault))
      .map(QuotationExchangeDate.apply)

  val quotationNegotiationCodeGen: Gen[QuotationNegotiationCode] =
    Gen.asciiPrintableStr.map(QuotationNegotiationCode.apply)

  val quotationOpeningPriceGen: Gen[QuotationOpeningPrice] =
    Gen.posNum[BigDecimal].map(QuotationOpeningPrice.apply)

  val quotationClosingPriceGen: Gen[QuotationClosingPrice] =
    Gen.posNum[BigDecimal].map(QuotationClosingPrice.apply)

  val equalQuotationGen: Gen[Quotation] =
    for {
      exchangeDate    <- quotationExchangeDateGen
      negotiationCode <- quotationNegotiationCodeGen
      price           <- Gen.posNum[BigDecimal]
    } yield Quotation(exchangeDate, negotiationCode, QuotationOpeningPrice(price), QuotationClosingPrice(price))

  val lessQuotationGen: Gen[Quotation] =
    for {
      exchangeDate    <- quotationExchangeDateGen
      negotiationCode <- quotationNegotiationCodeGen
      opening         <- Gen.chooseNum[BigDecimal](10_001, 100_000)
      closing         <- Gen.chooseNum[BigDecimal](0, 10_000)
    } yield Quotation(exchangeDate, negotiationCode, QuotationOpeningPrice(opening), QuotationClosingPrice(closing))

  val greaterQuotationGen: Gen[Quotation] =
    for {
      exchangeDate    <- quotationExchangeDateGen
      negotiationCode <- quotationNegotiationCodeGen
      opening         <- Gen.chooseNum[BigDecimal](0, 10_000)
      closing         <- Gen.chooseNum[BigDecimal](10_001, 100_000)
    } yield Quotation(exchangeDate, negotiationCode, QuotationOpeningPrice(opening), QuotationClosingPrice(closing))
}
