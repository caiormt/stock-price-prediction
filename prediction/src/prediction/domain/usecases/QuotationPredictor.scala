package prediction.domain.usecases

import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._

object QuotationPredictor {
  def apply[F[_]](implicit ev: QuotationPredictor[F]): QuotationPredictor[F] = ev
}

trait QuotationPredictor[F[_]] {
  def align(first: Vector[Quotation], second: Vector[Quotation]): F[AlgorithmAlignment]
  def predict(first: Vector[Quotation], second: Vector[Quotation]): F[Quotation]
}
