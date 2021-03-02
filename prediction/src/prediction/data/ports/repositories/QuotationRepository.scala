package prediction.data.ports.repositories

import prediction.domain.entities.quotation._

trait QuotationRepository[F[_]] {
  def load(): F[Vector[Quotation]]
}
