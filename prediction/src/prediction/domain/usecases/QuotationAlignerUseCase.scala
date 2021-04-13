package prediction.domain.usecases

import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._

trait QuotationAlignerUseCase[F[_]] {
  def align(actual: Vector[Quotation], reference: Vector[Quotation]): F[AlgorithmAlignment]
}
