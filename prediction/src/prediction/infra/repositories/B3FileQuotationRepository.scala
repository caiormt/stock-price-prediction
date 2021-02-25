package prediction.infra.repositories

import atto.fs2.Pipes._

import cats.effect._

import fs2._

import prediction.data.models.b3._
import prediction.data.parsers.b3._
import prediction.data.ports.repositories._
import prediction.domain.entities.quotation._

import java.nio.file._
import java.{ time => jt }

final class B3FileQuotationRepository[F[_]: Sync: ContextShift](path: Path, blocker: Blocker)
    extends QuotationRepository[F] {

  override def load(): F[List[Quotation]] =
    io.file
      .readAll[F](path, blocker, chunkSize = 1024)
      .through(text.utf8Decode)
      .through(text.lines)
      .through(parseN(entry))
      .collect(registerToQuotation)
      .compile
      .toList

  private val registerToQuotation: PartialFunction[Entry, Quotation] = {
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
