package prediction.domain.usecases

import prediction.domain.entities._
import prediction.domain.entities.quotation._

trait QuotationPredictorUseCase[F[_]] {
  def predict(actual: Vector[Quotation], reference: Vector[Quotation]): F[Option[Alphabet]]
}
