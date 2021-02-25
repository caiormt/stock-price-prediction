package prediction.domain.entities

import cats.implicits._

import cats.effect._

import weaver._

import prediction.domain.entities.algorithm._

object AlgorithmSpec extends SimpleIOSuite {
  pureTest("AlgorithmToken.Empty must use Alphabet.Empty") {
    expect(AlgorithmToken.Empty === AlgorithmToken(Alphabet.Empty))
  }

  // -----

  test("AlgorithmSequence(index) access must start by 0") {
    val sequence = AlgorithmSequence(Vector(AlgorithmToken(Alphabet.Positive)))
    IO(sequence(0)).attempt.map(either => expect(either.isLeft))
  }

  pureTest("AlgorithmSequence(index) access must index by 0") {
    val sequence = AlgorithmSequence(
      Vector(
        AlgorithmToken(Alphabet.Positive),
        AlgorithmToken(Alphabet.Negative),
        AlgorithmToken(Alphabet.Negative)
      )
    )
    expect(sequence(1) === AlgorithmToken(Alphabet.Positive))
  }

  // -----

  pureTest("AlgorithmScore.Empty must be AlgorithmScore(0)") {
    expect(AlgorithmScore.Empty === AlgorithmScore(0L))
  }

  pureTest("AlgorithmScore must have + operation") {
    val sum = AlgorithmScore(10L) + AlgorithmScore(5L)
    expect(sum === AlgorithmScore(15L))
  }

  pureTest("AlgorithmScore must have * operation") {
    val sum = AlgorithmScore(2L) * AlgorithmScore(4L)
    expect(sum === AlgorithmScore(8L))
  }

  pureTest("AlgorithmScore must have * operation with scalar") {
    val sum = AlgorithmScore(2L) * 4L
    expect(sum === AlgorithmScore(8L))
  }

  pureTest("AlgorithmScore must have * operation with scalar conversion") {
    val sum = AlgorithmScore(2L) * 4
    expect(sum === AlgorithmScore(8L))
  }
}
