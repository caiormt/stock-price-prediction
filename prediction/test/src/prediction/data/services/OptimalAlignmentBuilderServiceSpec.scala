package prediction.data.services

import cats._

import cats.effect._

import munit._

import prediction.domain.entities.Alphabet._
import prediction.domain.entities.algorithm._

import prediction.adapter.breeze.algorithm._
import prediction.adapter.services._

import breeze.linalg._

final class OptimalAlignmentBuilderServiceSpec extends CatsEffectSuite {

  private val matrix  = Eval.later(new BreezeMatrixAdapter[IO, AlgorithmScore])
  private val service = Eval.later {
    val matrix = new BreezeMatrixAdapter[IO, AlgorithmScore]
    new OptimalAlignmentBuilderService[IO, DenseMatrix](matrix, new ScoreCalculatorService)
  }

  test("should init first row with penalty") {
    val sequence = AlgorithmSequence(
      scala.collection.immutable.Vector(
        AlgorithmToken(Draw),
        AlgorithmToken(Positive),
        AlgorithmToken(Negative)
      )
    )

    val operation = for {
      M <- matrix.value.empty(4, 4)
      _ <- service.value.initFirstRowPenalty(sequence)(M)
    } yield M

    val expected = DenseMatrix(
      (AlgorithmScore(0), AlgorithmScore(-2), AlgorithmScore(-4), AlgorithmScore(-6)),
      (AlgorithmScore(0), AlgorithmScore(+0), AlgorithmScore(+0), AlgorithmScore(+0)),
      (AlgorithmScore(0), AlgorithmScore(+0), AlgorithmScore(+0), AlgorithmScore(+0)),
      (AlgorithmScore(0), AlgorithmScore(+0), AlgorithmScore(+0), AlgorithmScore(+0))
    )

    assertIO(operation, expected)
  }

  test("should fill matrix when both are different") {
    val leftSequence  = AlgorithmSequence(
      scala.collection.immutable.Vector(
        AlgorithmToken(Positive),
        AlgorithmToken(Positive),
        AlgorithmToken(Positive)
      )
    )
    val rightSequence = AlgorithmSequence(
      scala.collection.immutable.Vector(
        AlgorithmToken(Negative),
        AlgorithmToken(Negative),
        AlgorithmToken(Negative)
      )
    )

    val operation = for {
      M <- matrix.value.empty(4, 4)
      _ <- service.value.fillMatrix(leftSequence, rightSequence)(M)
    } yield M

    val expected = DenseMatrix(
      (AlgorithmScore(0), AlgorithmScore(+0), AlgorithmScore(+0), AlgorithmScore(+0)),
      (AlgorithmScore(0), AlgorithmScore(-1), AlgorithmScore(-1), AlgorithmScore(-1)),
      (AlgorithmScore(0), AlgorithmScore(-1), AlgorithmScore(-2), AlgorithmScore(-2)),
      (AlgorithmScore(0), AlgorithmScore(-1), AlgorithmScore(-2), AlgorithmScore(-3))
    )

    assertIO(operation, expected)
  }

  test("should fill matrix when both are equals") {
    val leftSequence  = AlgorithmSequence(
      scala.collection.immutable.Vector(
        AlgorithmToken(Positive),
        AlgorithmToken(Positive),
        AlgorithmToken(Positive)
      )
    )
    val rightSequence = AlgorithmSequence(
      scala.collection.immutable.Vector(
        AlgorithmToken(Positive),
        AlgorithmToken(Positive),
        AlgorithmToken(Positive)
      )
    )

    val operation = for {
      M <- matrix.value.empty(4, 4)
      _ <- service.value.fillMatrix(leftSequence, rightSequence)(M)
    } yield M

    val expected = DenseMatrix(
      (AlgorithmScore(0), AlgorithmScore(+0), AlgorithmScore(+0), AlgorithmScore(+0)),
      (AlgorithmScore(0), AlgorithmScore(+2), AlgorithmScore(+2), AlgorithmScore(+2)),
      (AlgorithmScore(0), AlgorithmScore(+2), AlgorithmScore(+4), AlgorithmScore(+4)),
      (AlgorithmScore(0), AlgorithmScore(+2), AlgorithmScore(+4), AlgorithmScore(+6))
    )

    assertIO(operation, expected)
  }

  test("should completly build matrix when both are different") {
    val leftSequence  = AlgorithmSequence(
      scala.collection.immutable.Vector(
        AlgorithmToken(Positive),
        AlgorithmToken(Positive),
        AlgorithmToken(Positive)
      )
    )
    val rightSequence = AlgorithmSequence(
      scala.collection.immutable.Vector(
        AlgorithmToken(Negative),
        AlgorithmToken(Negative),
        AlgorithmToken(Negative)
      )
    )

    val operation = service.value.build(leftSequence, rightSequence).unsafeRunSync()

    val expected = DenseMatrix(
      (AlgorithmScore(0), AlgorithmScore(-2), AlgorithmScore(-4), AlgorithmScore(-6)),
      (AlgorithmScore(0), AlgorithmScore(-1), AlgorithmScore(-3), AlgorithmScore(-5)),
      (AlgorithmScore(0), AlgorithmScore(-1), AlgorithmScore(-2), AlgorithmScore(-4)),
      (AlgorithmScore(0), AlgorithmScore(-1), AlgorithmScore(-2), AlgorithmScore(-3))
    )

    assertEquals(operation.cols, 4)
    assertEquals(operation.rows, 4)
    assertEquals(operation, expected)
  }

  test("should completly build matrix when both are equals") {
    val leftSequence  = AlgorithmSequence(
      scala.collection.immutable.Vector(
        AlgorithmToken(Positive),
        AlgorithmToken(Positive),
        AlgorithmToken(Positive)
      )
    )
    val rightSequence = AlgorithmSequence(
      scala.collection.immutable.Vector(
        AlgorithmToken(Positive),
        AlgorithmToken(Positive),
        AlgorithmToken(Positive)
      )
    )

    val operation = service.value.build(leftSequence, rightSequence).unsafeRunSync()

    val expected = DenseMatrix(
      (AlgorithmScore(0), AlgorithmScore(-2), AlgorithmScore(-4), AlgorithmScore(-6)),
      (AlgorithmScore(0), AlgorithmScore(+2), AlgorithmScore(+0), AlgorithmScore(-2)),
      (AlgorithmScore(0), AlgorithmScore(+2), AlgorithmScore(+4), AlgorithmScore(+2)),
      (AlgorithmScore(0), AlgorithmScore(+2), AlgorithmScore(+4), AlgorithmScore(+6))
    )

    assertEquals(operation.cols, 4)
    assertEquals(operation.rows, 4)
    assertEquals(operation, expected)
  }
}
