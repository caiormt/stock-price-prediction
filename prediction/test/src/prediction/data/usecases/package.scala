package prediction.data

import cats.implicits._

import cats.effect._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._
import prediction.infra.services._

import breeze.linalg._
import breeze.linalg.support._
import org.scalacheck._

import java.{ time => jt }

package object usecases {

  def denseMatrix[R](rows: R*)(implicit lr: LiteralRow[R, Long]): IO[DenseMatrix[AlgorithmScore]] =
    IO(DenseMatrix[R, Long](rows: _*).map(AlgorithmScore.apply))

  // -----

  val nonEmptyAlphabetGen: Gen[Alphabet] =
    Gen.oneOf(Alphabet.Positive, Alphabet.Draw, Alphabet.Negative)

  val nonEmptyTupledDistinctAlphabetGen: Gen[(Alphabet, Alphabet)] =
    for {
      a <- nonEmptyAlphabetGen
      b <- nonEmptyAlphabetGen.suchThat(b => a =!= b)
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
      price           <- Gen.chooseNum[BigDecimal](1, Int.MaxValue)
      opening         <- Gen.chooseNum[BigDecimal](price + 1, price * 2)
      closing         <- Gen.chooseNum[BigDecimal](0, price)
    } yield Quotation(exchangeDate, negotiationCode, QuotationOpeningPrice(opening), QuotationClosingPrice(closing))

  val greaterQuotationGen: Gen[Quotation] =
    for {
      exchangeDate    <- quotationExchangeDateGen
      negotiationCode <- quotationNegotiationCodeGen
      price           <- Gen.chooseNum[BigDecimal](1, Int.MaxValue)
      opening         <- Gen.chooseNum[BigDecimal](0, price)
      closing         <- Gen.chooseNum[BigDecimal](price + 1, price * 2)
    } yield Quotation(exchangeDate, negotiationCode, QuotationOpeningPrice(opening), QuotationClosingPrice(closing))
}
