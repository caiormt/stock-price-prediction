package prediction.data.services

import cats._

import cats.effect._

import munit._

import prediction.domain.entities.Alphabet._
import prediction.domain.entities.algorithm._

final class ScoreCalculatorServiceSpec extends CatsEffectSuite {

  private val service = Eval.later(new ScoreCalculatorService[IO])

  test("should return -2 when empty on left") {
    val leftEmptyScore1 = service.value.calculate(AlgorithmToken.Empty, AlgorithmToken(Positive1))
    assertIO(leftEmptyScore1, AlgorithmScore(-2L))

    val leftEmptyScore2 = service.value.calculate(AlgorithmToken.Empty, AlgorithmToken(Draw))
    assertIO(leftEmptyScore2, AlgorithmScore(-2L))

    val leftEmptyScore3 = service.value.calculate(AlgorithmToken.Empty, AlgorithmToken(Negative1))
    assertIO(leftEmptyScore3, AlgorithmScore(-2L))
  }

  test("should return -2 when empty on right") {
    val rightEmptyScore1 = service.value.calculate(AlgorithmToken(Positive1), AlgorithmToken.Empty)
    assertIO(rightEmptyScore1, AlgorithmScore(-2L))

    val rightEmptyScore2 = service.value.calculate(AlgorithmToken(Draw), AlgorithmToken.Empty)
    assertIO(rightEmptyScore2, AlgorithmScore(-2L))

    val rightEmptyScore3 = service.value.calculate(AlgorithmToken(Negative1), AlgorithmToken.Empty)
    assertIO(rightEmptyScore3, AlgorithmScore(-2L))
  }

  test("should return -2 when empty on both sides") {
    val rightEmptyScore1 = service.value.calculate(AlgorithmToken.Empty, AlgorithmToken.Empty)
    assertIO(rightEmptyScore1, AlgorithmScore(-2L))
  }

  test("should return -1 when tokens non equal") {
    val leftPositive1Score1 = service.value.calculate(AlgorithmToken(Positive1), AlgorithmToken(Draw))
    assertIO(leftPositive1Score1, AlgorithmScore(-1L))

    val leftPositive1Score2 = service.value.calculate(AlgorithmToken(Positive1), AlgorithmToken(Negative1))
    assertIO(leftPositive1Score2, AlgorithmScore(-1L))

    val leftDrawScore1 = service.value.calculate(AlgorithmToken(Draw), AlgorithmToken(Positive1))
    assertIO(leftDrawScore1, AlgorithmScore(-1L))

    val leftDrawScore2 = service.value.calculate(AlgorithmToken(Draw), AlgorithmToken(Negative1))
    assertIO(leftDrawScore2, AlgorithmScore(-1L))

    val leftNegative1Score1 = service.value.calculate(AlgorithmToken(Negative1), AlgorithmToken(Positive1))
    assertIO(leftNegative1Score1, AlgorithmScore(-1L))

    val leftNegative1Score2 = service.value.calculate(AlgorithmToken(Negative1), AlgorithmToken(Draw))
    assertIO(leftNegative1Score2, AlgorithmScore(-1L))
  }

  test("should return 2 when tokens equals") {
    val positive1Score = service.value.calculate(AlgorithmToken(Positive1), AlgorithmToken(Positive1))
    assertIO(positive1Score, AlgorithmScore(2L))

    val drawScore = service.value.calculate(AlgorithmToken(Draw), AlgorithmToken(Draw))
    assertIO(drawScore, AlgorithmScore(2L))

    val negative1Score = service.value.calculate(AlgorithmToken(Negative1), AlgorithmToken(Negative1))
    assertIO(negative1Score, AlgorithmScore(2L))
  }
}
