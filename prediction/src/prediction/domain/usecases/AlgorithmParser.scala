package prediction.domain.usecases

import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._

object AlgorithmParser {
  def apply[F[_]](implicit ev: AlgorithmParser[F]): AlgorithmParser[F] = ev
}

trait AlgorithmParser[F[_]] {
  def parseQuotation(quotation: Quotation): F[AlgorithmToken]
}
