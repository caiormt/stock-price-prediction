package prediction.data.services

import cats._
import cats.implicits._

import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._
import prediction.domain.usecases._

final class QuotationAlignerService[F[_]: Monad: Parallel](
    algorithmParser: AlgorithmParserUseCase[F, Quotation, AlgorithmToken],
    optimalAlignment: OptimalAlignmentAlignerUseCase[F, AlgorithmSequence, AlgorithmAlignment]
) extends QuotationAlignerUseCase[F] {

  override def align(actual: Vector[Quotation], reference: Vector[Quotation]): F[AlgorithmAlignment] =
    for {
      (actual, reference) <- (parseQuotations(actual), parseQuotations(reference)).parTupled
      alignment           <- optimalAlignment.align(reference, actual)
    } yield alignment

  def parseQuotations(quotations: Vector[Quotation]): F[AlgorithmSequence] =
    quotations.parTraverse(algorithmParser.parse).map(AlgorithmSequence.apply)
}
