package prediction.data.usecases

import cats._
import cats.implicits._

import cats.effect._

import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._
import prediction.domain.usecases._

object DefaultQuotationPredictor {
  def make[F[_]: Sync: Parallel, M[_]](
      algorithmParser: AlgorithmParser[F],
      optimalAlignment: OptimalAlignment[F, M]
  ): F[DefaultQuotationPredictor[F, M]] =
    Sync[F].delay(new DefaultQuotationPredictor[F, M](algorithmParser, optimalAlignment))
}

final class DefaultQuotationPredictor[F[_]: Sync: Parallel, M[_]] private (
    algorithmParser: AlgorithmParser[F],
    optimalAlignment: OptimalAlignment[F, M]
) extends QuotationPredictor[F] {

  override def align(first: Vector[Quotation], second: Vector[Quotation]): F[AlgorithmAlignment] =
    for {
      (fst, snd) <- (parseQuotations(first), parseQuotations(second)).parTupled
      M          <- optimalAlignment.buildMatrix(fst, snd)
      alignment  <- optimalAlignment.align(fst, snd, M)
    } yield alignment

  override def predict(first: Vector[Quotation], second: Vector[Quotation]): F[Quotation] =
    for {
      (fst, snd) <- (parseQuotations(first), parseQuotations(second)).parTupled
      M          <- optimalAlignment.buildMatrix(fst, snd)
      (i, _)     <- optimalAlignment.coordinatesMaxValue(M)
      quotation  <- Sync[F].delay(first.at(i + 1)).adaptError {
                      case _: IndexOutOfBoundsException => NotEnoughDataToPredict
                    }
    } yield quotation

  private def parseQuotations(quotations: Vector[Quotation]): F[AlgorithmSequence] =
    quotations.parTraverse(algorithmParser.parseQuotation).map(AlgorithmSequence.apply)
}
