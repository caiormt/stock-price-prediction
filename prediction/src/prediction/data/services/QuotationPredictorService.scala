package prediction.data.services

import cats._
import cats.implicits._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._
import prediction.domain.usecases._

final class QuotationPredictorService[F[_]: Monad: Parallel, Matrix[_]](
    algorithmParser: AlgorithmParserUseCase[F, Quotation, AlgorithmToken],
    optimalAlignment: OptimalAlignmentBuilderUseCase[F, AlgorithmSequence, Matrix, AlgorithmScore],
    alignmentFinder: AlignmentFinderUseCase[F, Matrix, AlgorithmScore]
) extends QuotationPredictorUseCase[F] {

  override def predict(actual: Vector[Quotation], reference: Vector[Quotation]): F[Option[Alphabet]] =
    for {
      (actual, reference) <- (parseQuotations(actual), parseQuotations(reference)).parTupled
      matrix              <- optimalAlignment.build(reference, actual)
      (i, _)              <- alignmentFinder.findCoordinates(matrix)
      predicted           <- Applicative[F].pure(reference.liftAt(i + 1).map(_.value))
    } yield predicted

  def parseQuotations(quotations: Vector[Quotation]): F[AlgorithmSequence] =
    quotations.parTraverse(algorithmParser.parse).map(AlgorithmSequence.apply)
}
