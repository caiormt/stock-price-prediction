package prediction.data.usecases.b3

import cats._
import cats.implicits._

import cats.effect._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._
import prediction.domain.usecases._

object B3ScoreCalculator {
  def make[F[_]: Sync]: F[B3ScoreCalculator[F]] =
    Sync[F].delay(new B3ScoreCalculator[F])
}

final class B3ScoreCalculator[F[_]: Applicative] extends ScoreCalculator[F] {

  import Alphabet._

  override def calculate(first: AlgorithmToken, second: AlgorithmToken): F[AlgorithmScore] = {
    def decideScore(first: AlgorithmToken, second: AlgorithmToken): Long =
      (first.value, second.value) match {
        case (Empty, _)        => -2L
        case (_, Empty)        => -2L
        case (a, b) if a === b => +2L
        case _                 => +1L
      }

    Applicative[F]
      .pure(decideScore(first, second))
      .map(AlgorithmScore.apply)
  }
}
