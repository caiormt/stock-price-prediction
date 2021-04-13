package prediction.data.services

import cats._

import cats.effect._

import munit._

import prediction.domain.entities.Alphabet._
import prediction.domain.entities.algorithm._

final class ScoreCalculatorServiceSpec extends CatsEffectSuite {

  private val service = Eval.later(new ScoreCalculatorService[IO])

  test("should return -2 when empty on left") {
    val leftEmptyScore1 = service.value.calculate(AlgorithmToken.Empty, AlgorithmToken(Positive))
    assertIO(leftEmptyScore1, AlgorithmScore(-2L))

    val leftEmptyScore2 = service.value.calculate(AlgorithmToken.Empty, AlgorithmToken(Draw))
    assertIO(leftEmptyScore2, AlgorithmScore(-2L))

    val leftEmptyScore3 = service.value.calculate(AlgorithmToken.Empty, AlgorithmToken(Negative))
    assertIO(leftEmptyScore3, AlgorithmScore(-2L))
  }

  test("should return -2 when empty on right") {
    val rightEmptyScore1 = service.value.calculate(AlgorithmToken(Positive), AlgorithmToken.Empty)
    assertIO(rightEmptyScore1, AlgorithmScore(-2L))

    val rightEmptyScore2 = service.value.calculate(AlgorithmToken(Draw), AlgorithmToken.Empty)
    assertIO(rightEmptyScore2, AlgorithmScore(-2L))

    val rightEmptyScore3 = service.value.calculate(AlgorithmToken(Negative), AlgorithmToken.Empty)
    assertIO(rightEmptyScore3, AlgorithmScore(-2L))
  }

  test("should return -2 when empty on both sides") {
    val rightEmptyScore1 = service.value.calculate(AlgorithmToken.Empty, AlgorithmToken.Empty)
    assertIO(rightEmptyScore1, AlgorithmScore(-2L))
  }

  test("should return -1 when tokens non equal") {
    val leftPositiveScore1 = service.value.calculate(AlgorithmToken(Positive), AlgorithmToken(Draw))
    assertIO(leftPositiveScore1, AlgorithmScore(-1L))

    val leftPositiveScore2 = service.value.calculate(AlgorithmToken(Positive), AlgorithmToken(Negative))
    assertIO(leftPositiveScore2, AlgorithmScore(-1L))

    val leftDrawScore1 = service.value.calculate(AlgorithmToken(Draw), AlgorithmToken(Positive))
    assertIO(leftDrawScore1, AlgorithmScore(-1L))

    val leftDrawScore2 = service.value.calculate(AlgorithmToken(Draw), AlgorithmToken(Negative))
    assertIO(leftDrawScore2, AlgorithmScore(-1L))

    val leftNegativeScore1 = service.value.calculate(AlgorithmToken(Negative), AlgorithmToken(Positive))
    assertIO(leftNegativeScore1, AlgorithmScore(-1L))

    val leftNegativeScore2 = service.value.calculate(AlgorithmToken(Negative), AlgorithmToken(Draw))
    assertIO(leftNegativeScore2, AlgorithmScore(-1L))
  }

  test("should return 2 when tokens equals") {
    val positiveScore = service.value.calculate(AlgorithmToken(Positive), AlgorithmToken(Positive))
    assertIO(positiveScore, AlgorithmScore(2L))

    val drawScore = service.value.calculate(AlgorithmToken(Draw), AlgorithmToken(Draw))
    assertIO(drawScore, AlgorithmScore(2L))

    val negativeScore = service.value.calculate(AlgorithmToken(Negative), AlgorithmToken(Negative))
    assertIO(negativeScore, AlgorithmScore(2L))
  }
}
