package prediction.data.services

import cats._
import cats.implicits._

import prediction.domain.entities.algorithm._
import prediction.domain.usecases._

final class ScoreCalculatorService[F[_]: Applicative]
    extends ScoreCalculatorUseCase[F, AlgorithmToken, AlgorithmScore] {

  override def calculate(left: AlgorithmToken, right: AlgorithmToken): F[AlgorithmScore] = {
    def decideScore(left: AlgorithmToken, right: AlgorithmToken): Long =
      (left, right) match {
        case (AlgorithmToken.Empty, _)       => -2L
        case (_, AlgorithmToken.Empty)       => -2L
        case (left, right) if left === right => +2L
        case _                               => -1L
      }

    Applicative[F]
      .pure(decideScore(left, right))
      .map(AlgorithmScore.apply)
  }
}
