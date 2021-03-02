package prediction.presentation.controllers

import cats._
import cats.implicits._

import cats.effect._

import prediction.data.ports.repositories._
import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._
import prediction.domain.usecases._

object ComandLineController {
  def make[F[_]: Sync: Parallel](quotationPrediction: QuotationPredictor[F]): F[ComandLineController[F]] =
    Sync[F].delay(new ComandLineController[F](quotationPrediction))
}

final class ComandLineController[F[_]: Sync: Parallel] private (quotationPrediction: QuotationPredictor[F]) {
  def align(
      firstQuotationRepository: QuotationRepository[F],
      secondQuotationRepository: QuotationRepository[F]
  ): F[AlgorithmAlignment] =
    for {
      (first, second) <- (firstQuotationRepository.load(), secondQuotationRepository.load()).parTupled
      alignment       <- quotationPrediction.align(first, second)
    } yield alignment

  def predict(
      firstQuotationRepository: QuotationRepository[F],
      secondQuotationRepository: QuotationRepository[F]
  ): F[Quotation] =
    for {
      (first, second) <- (firstQuotationRepository.load(), secondQuotationRepository.load()).parTupled
      quotation       <- quotationPrediction.predict(first, second)
    } yield quotation
}
