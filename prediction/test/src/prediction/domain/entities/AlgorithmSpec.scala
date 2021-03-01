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

  pureTest("AlgorithmSequence must have size operation") {
    expect(AlgorithmSequence(Vector.empty).size === 0)
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

  // -----

  pureTest("RowIndex.Empty must be RowIndex(0)") {
    expect(RowIndex.Empty === RowIndex(0))
  }

  pureTest("RowIndex must have - operation") {
    expect(RowIndex(5) - 1 === RowIndex(4))
  }

  pureTest("ColumnIndex.Empty must be ColumnIndex(0)") {
    expect(ColumnIndex.Empty === ColumnIndex(0))
  }

  pureTest("ColumnIndex must have - operation") {
    expect(ColumnIndex(5) - 1 === ColumnIndex(4))
  }

  pureTest("SequenceIndex.Empty must be SequenceIndex(0)") {
    expect(SequenceIndex.Empty === SequenceIndex(0))
  }

  pureTest("SequenceIndex must have + operation") {
    expect(SequenceIndex(5) + 1 === SequenceIndex(6))
  }

  pureTest("Step must have Up operation") {
    val step = Step(RowIndex(5), ColumnIndex(5), SequenceIndex(0))
    expect(step.up === Step(RowIndex(4), ColumnIndex(5), SequenceIndex(1)))
  }

  pureTest("Step must have UpLeft operation") {
    val step = Step(RowIndex(5), ColumnIndex(5), SequenceIndex(0))
    expect(step.upLeft === Step(RowIndex(4), ColumnIndex(4), SequenceIndex(1)))
  }

  pureTest("Step must have Left operation") {
    val step = Step(RowIndex(5), ColumnIndex(5), SequenceIndex(0))
    expect(step.left === Step(RowIndex(5), ColumnIndex(4), SequenceIndex(1)))
  }
}
