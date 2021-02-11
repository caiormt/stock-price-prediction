package prediction.presentation.controllers

import cats._
import cats.implicits._

import cats.effect._

import prediction.domain.entities.algorithm._
import prediction.domain.usecases._

object ComandLineController {
  def make[F[_]: Sync, M[_]](optimalAlignment: OptimalAlignment[F, M]): F[ComandLineController[F, M]] =
    Sync[F].delay(new ComandLineController[F, M](optimalAlignment))
}

final class ComandLineController[F[_]: FlatMap, M[_]](optimalAlignment: OptimalAlignment[F, M]) {
  def align(first: AlgorithmSequence, second: AlgorithmSequence): F[AlgorithmAlignment] =
    for {
      M         <- optimalAlignment.buildMatrix(first, second)
      alignment <- optimalAlignment.align(first, second, M)
    } yield alignment
}
