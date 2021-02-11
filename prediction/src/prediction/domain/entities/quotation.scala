package prediction.domain.entities

import cats._
import cats.derived._
import cats.implicits._

import io.chrisdavenport.cats.time._

import io.estatico.newtype.macros._

import java.{ time => jt }

object quotation {
  @newtype case class QuotationExchangeDate(value: jt.LocalDateTime)
  object QuotationExchangeDate {
    implicit val eqForQuotationExchangeDate: Eq[QuotationExchangeDate]     = deriving
    implicit val showForQuotationExchangeDate: Show[QuotationExchangeDate] = deriving
  }

  @newtype case class QuotationNegotiationCode(value: String)
  object QuotationNegotiationCode {
    implicit val eqForQuotationNegotiationCode: Eq[QuotationNegotiationCode]     = deriving
    implicit val showForQuotationNegotiationCode: Show[QuotationNegotiationCode] = deriving
  }

  @newtype case class QuotationOpeningPrice(value: BigDecimal)
  object QuotationOpeningPrice {
    implicit val orderForQuotationOpeningPrice: Order[QuotationOpeningPrice] = deriving
    implicit val showForQuotationOpeningPrice: Show[QuotationOpeningPrice]   = deriving
  }

  @newtype case class QuotationClosingPrice(value: BigDecimal)
  object QuotationClosingPrice {
    implicit val orderForQuotationClosingPrice: Order[QuotationClosingPrice] = deriving
    implicit val showForQuotationClosingPrice: Show[QuotationClosingPrice]   = deriving
  }

  final case class Quotation(
      exchangeDate: QuotationExchangeDate,
      negotiationCode: QuotationNegotiationCode,
      openingPrice: QuotationOpeningPrice,
      closingPrice: QuotationClosingPrice
  )
  object Quotation {
    implicit val eqForQuotation: Eq[Quotation]     = semiauto.eq
    implicit val showForQuotation: Show[Quotation] = semiauto.show
  }
}
