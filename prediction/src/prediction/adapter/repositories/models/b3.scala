package prediction.adapter.repositories.models

import prediction.domain.entities.quotation._

import java.{ time => jt }

object b3 {

  /**
    * Defines the base type of the entries that could possibly be found in the B3 file.
    */
  sealed abstract class Entry extends Product with Serializable

  /**
    * Header entry type.
    * Must be the first item in the file.
    * Defines the metadata of the file.
    *
    * @param fileName The name of the file containing this data.
    * @param sourceCode The name of the Stock Market.
    * @param fileGenerationDate The date when this file was generated.
    */
  final case class Header(
      fileName: String,
      sourceCode: String,
      fileGenerationDate: jt.LocalDate
  ) extends Entry

  /**
    * Register entry type.
    * Each row is a register defining the info about the stock in the specified `dateOfExchange`.
    *
    * @param dateOfExchange The date when this exchange ocurred.
    * @param codNeg Paper negotiation code - the code of this paper in the Market.
    * @param nomRes Responsible company - Abbreviated name of the company that issued the paper.
    * @param especi Paper specification.
    * @param modRef Reference currency.
    * @param preAbe Market paper opening floor price.
    * @param preMax Market paper highest floor price.
    * @param preMin Market paper lowest floor price.
    * @param preMed Market paper average floor price.
    * @param preUlt Market paper last negotiated price.
    * @param preOfc Market paper best purchase offer price.
    * @param preOfv Market paper best sale offer price.
    * @param totNeg Number of trades conducted with the market paper.
    * @param quaTot Total quantity of titles traded with this market paper.
    * @param volTot Total volume of titles negotiated with this market paper.
    */
  final case class Register(
      dateOfExchange: jt.LocalDate,
      codNeg: String,
      nomRes: String,
      especi: String,
      modRef: String,
      preAbe: BigDecimal,
      preMax: BigDecimal,
      preMin: BigDecimal,
      preMed: BigDecimal,
      preUlt: BigDecimal,
      preOfc: BigDecimal,
      preOfv: BigDecimal,
      totNeg: Int,
      quaTot: Int,
      volTot: BigDecimal
  ) extends Entry

  /**
    * Trailer entry type.
    * Must be the last item in the file.
    * Defines the metadata of the file and the total number of entries before it.
    *
    * @param fileName The name of the file containing this data.
    * @param sourceCode The name of the Stock Market.
    * @param fileGenerationDate The date when this file was generated.
    * @param totalRegisters The number of records before this item.
    */
  final case class Trailer(
      fileName: String,
      sourceCode: String,
      fileGenerationDate: jt.LocalDate,
      totalRegisters: Long
  ) extends Entry

  def collector: PartialFunction[Entry, Quotation] = {
    case r: Register =>
      Quotation(
        QuotationExchangeDate(
          jt.LocalDateTime.of(r.dateOfExchange, jt.LocalTime.MIDNIGHT)
        ),
        QuotationNegotiationCode(r.codNeg),
        QuotationOpeningPrice(r.preAbe),
        QuotationClosingPrice(r.preUlt)
      )
  }
}
