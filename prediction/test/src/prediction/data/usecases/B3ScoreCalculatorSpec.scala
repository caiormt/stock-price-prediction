package prediction.data.usecases

import cats.implicits._

import cats.effect._

import weaver._
import weaver.scalacheck._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._

object B3ScoreCalculatorSpec extends SimpleIOSuite with Checkers {

  override def checkConfig: CheckConfig =
    CheckConfig.default.copy(maximumDiscardRatio = 80)

  test("Both tokens empty") {
    for {
      calculator <- B3ScoreCalculator.make[IO]
      score      <- calculator.calculate(AlgorithmToken(Alphabet.Empty), AlgorithmToken(Alphabet.Empty))
    } yield expect(score === AlgorithmScore(-2L))
  }

  test("Left token empty") {
    forall(nonEmptyAlphabetGen) { alphabet: Alphabet =>
      for {
        calculator <- B3ScoreCalculator.make[IO]
        score      <- calculator.calculate(AlgorithmToken(Alphabet.Empty), AlgorithmToken(alphabet))
      } yield expect(score === AlgorithmScore(-2L))
    }
  }

  test("Right token empty") {
    forall(nonEmptyAlphabetGen) { alphabet: Alphabet =>
      for {
        calculator <- B3ScoreCalculator.make[IO]
        score      <- calculator.calculate(AlgorithmToken(alphabet), AlgorithmToken(Alphabet.Empty))
      } yield expect(score === AlgorithmScore(-2L))
    }
  }

  test("Both tokens non empty equal") {
    forall(nonEmptyAlphabetGen) { alphabet: Alphabet =>
      for {
        calculator <- B3ScoreCalculator.make[IO]
        score      <- calculator.calculate(AlgorithmToken(alphabet), AlgorithmToken(alphabet))
      } yield expect(score === AlgorithmScore(+2L))
    }
  }

  test("Both tokens non empty non equal") {
    forall(nonEmptyTupledAlphabetGen) {
      case (alphabet1, alphabet2) =>
        for {
          calculator <- B3ScoreCalculator.make[IO]
          score      <- calculator.calculate(AlgorithmToken(alphabet1), AlgorithmToken(alphabet2))
        } yield expect(score === AlgorithmScore(+1L))
    }
  }
}
