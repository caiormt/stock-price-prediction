package prediction.data.ports

import prediction.domain.entities.quotation._

trait QuotationRepository[F[_]] {
  def load(): F[List[Quotation]]
}
