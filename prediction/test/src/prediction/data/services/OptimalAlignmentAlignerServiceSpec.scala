package prediction.data.services

import cats._

import cats.effect._

import munit._

import prediction.domain.entities.Alphabet._
import prediction.domain.entities.algorithm._

import prediction.adapter.breeze.algorithm._
import prediction.adapter.services._

import breeze.linalg.{ DenseMatrix, DenseVector }
import natchez.Trace.Implicits._

final class OptimalAlignmentAlignerServiceSpec extends CatsEffectSuite {

  private val matrix           = Eval.later(new BreezeMatrixAdapter[IO, AlgorithmScore])
  private val vector           = Eval.later(new BreezeVectorAdapter[IO, AlgorithmToken])
  private val optimalAlignment = Eval.later {
    val matrix = new BreezeMatrixAdapter[IO, AlgorithmScore]
    val score  = new ScoreCalculatorService[IO]
    new OptimalAlignmentBuilderService[IO, DenseMatrix](matrix, score)
  }
  private val service          = Eval.later {
    val matrix           = new BreezeMatrixAdapter[IO, AlgorithmScore]
    val vector           = new BreezeVectorAdapter[IO, AlgorithmToken]
    val score            = new ScoreCalculatorService[IO]
    val alignment        = new AlignmentFinderService[IO, DenseMatrix](matrix)
    val optimalAlignment = new OptimalAlignmentBuilderService[IO, DenseMatrix](matrix, score)
    new OptimalAlignmentAlignerService[IO, DenseMatrix, DenseVector](matrix, vector, score, alignment, optimalAlignment)
  }

  test("should create empty alignments") {
    service.value.createEmptyAlignments(5).map { alignments =>
      import alignments._
      assertEquals(s.size, 5)
      assertEquals(t.size, 5)
    }
  }

  test("should wrap sequences") {
    val left  = AlgorithmSequence(
      Vector(
        AlgorithmToken(Draw),
        AlgorithmToken(Positive1),
        AlgorithmToken(Negative1)
      )
    )
    val right = AlgorithmSequence(
      Vector(
        AlgorithmToken(Negative1),
        AlgorithmToken(Negative1),
        AlgorithmToken(Negative1)
      )
    )

    service.value.wrapSequences(left, right).map { sequences =>
      assertEquals(sequences.left, left)
      assertEquals(sequences.right, right)
    }
  }

  test("should find starting step") {
    for {
      (row, column) <- IO(3 -> 4)
      m             <- matrix.value.empty(5, 5)
      _             <- matrix.value.set(m, row, column, AlgorithmScore(5L))
      step          <- service.value.startingStep(m)
    } yield assertEquals(step, AlgorithmStep(row, column, 0))
  }

  test("should consume tail sequence until reach step to start algorithm") {
    val left  = AlgorithmSequence(
      Vector(
        AlgorithmToken(Draw),
        AlgorithmToken(Positive1),
        AlgorithmToken(Negative1)
      )
    )
    val right = AlgorithmSequence(
      Vector(
        AlgorithmToken(Negative1),
        AlgorithmToken(Negative1),
        AlgorithmToken(Negative1)
      )
    )

    val result = AlgorithmSequence(
      Vector(
        AlgorithmToken(Negative1),
        AlgorithmToken(Positive1),
        AlgorithmToken.Empty
      )
    )
    val empty  = AlgorithmSequence(Vector.fill(3)(AlgorithmToken.Empty))

    for {
      service    <- IO(service.value)
      m          <- matrix.value.empty(4, 4)
      alignments <- service.createEmptyAlignments(4)
      sequences  <- IO(service.Sequences(left, right))
      step       <- service.consumeTailSequence(m, alignments, sequences)(AlgorithmStep(1, 3))
      s          <- vector.value.toScalaVector(alignments.s).map(_.take(3)).map(AlgorithmSequence.apply)
      t          <- vector.value.toScalaVector(alignments.t).map(_.take(3)).map(AlgorithmSequence.apply)
    } yield assertEquals((s, t, step), (result, empty, AlgorithmStep(1, 3, 2)))
  }

  test("should build best alignments when both sequences are different") {
    val left  = AlgorithmSequence(
      Vector(
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1)
      )
    )
    val right = AlgorithmSequence(
      Vector(
        AlgorithmToken(Negative1),
        AlgorithmToken(Negative1),
        AlgorithmToken(Negative1)
      )
    )

    val leftResult  = Vector(
      AlgorithmToken(Positive1),
      AlgorithmToken(Positive1),
      AlgorithmToken(Positive1),
      AlgorithmToken.Empty
    )
    val rightResult = Vector(
      AlgorithmToken(Negative1),
      AlgorithmToken(Negative1),
      AlgorithmToken(Negative1),
      AlgorithmToken.Empty
    )

    for {
      service    <- IO(service.value)
      m          <- optimalAlignment.value.build(left, right)
      alignments <- service.createEmptyAlignments(4)
      sequences  <- IO(service.Sequences(left, right))
      step       <- service.buildBestAlignments(m, alignments, sequences)(AlgorithmStep(3, 3))
      s          <- vector.value.toScalaVector(alignments.s)
      t          <- vector.value.toScalaVector(alignments.t)
    } yield assertEquals((s, t, step), (leftResult, rightResult, AlgorithmStep(0, 0, 3)))
  }

  test("should build best alignments when sequences has partial match") {
    val left  = AlgorithmSequence(
      Vector(
        AlgorithmToken(Negative1),
        AlgorithmToken(Negative1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1)
      )
    )
    val right = AlgorithmSequence(
      Vector(
        AlgorithmToken(Negative1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1)
      )
    )

    val result = Vector(
      AlgorithmToken(Positive1),
      AlgorithmToken(Positive1),
      AlgorithmToken(Negative1),
      AlgorithmToken.Empty
    )

    for {
      service    <- IO(service.value)
      m          <- optimalAlignment.value.build(left, right)
      alignments <- service.createEmptyAlignments(12)
      sequences  <- IO(service.Sequences(left, right))
      step       <- service.buildBestAlignments(m, alignments, sequences)(AlgorithmStep(4, 3))
      s          <- vector.value.toScalaVector(alignments.s).map(_.take(4))
      t          <- vector.value.toScalaVector(alignments.t).map(_.take(4))
    } yield assertEquals((s, t, step), (result, result, AlgorithmStep(1, 0, 3)))
  }

  test("should build best alignments when both sequences are equals") {
    val left  = AlgorithmSequence(
      Vector(
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1)
      )
    )
    val right = AlgorithmSequence(
      Vector(
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1)
      )
    )

    val result = Vector(
      AlgorithmToken(Positive1),
      AlgorithmToken(Positive1),
      AlgorithmToken(Positive1),
      AlgorithmToken.Empty
    )

    for {
      service    <- IO(service.value)
      m          <- optimalAlignment.value.build(left, right)
      alignments <- service.createEmptyAlignments(4)
      sequences  <- IO(service.Sequences(left, right))
      step       <- service.buildBestAlignments(m, alignments, sequences)(AlgorithmStep(3, 3))
      s          <- vector.value.toScalaVector(alignments.s)
      t          <- vector.value.toScalaVector(alignments.t)
    } yield assertEquals((s, t, step), (result, result, AlgorithmStep(0, 0, 3)))
  }

  test("should consume remaining tokens from right sequence") {
    val left  = AlgorithmSequence(
      Vector(
        AlgorithmToken(Negative1),
        AlgorithmToken(Negative1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1)
      )
    )
    val right = AlgorithmSequence(
      Vector(
        AlgorithmToken(Negative1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Negative1)
      )
    )

    val result = Vector(
      AlgorithmToken(Negative1),
      AlgorithmToken(Positive1),
      AlgorithmToken(Positive1),
      AlgorithmToken(Negative1)
    )
    val empty  = Vector.fill(4)(AlgorithmToken.Empty)

    for {
      service    <- IO(service.value)
      alignments <- service.createEmptyAlignments(4)
      sequences  <- IO(service.Sequences(left, right))
      step       <- service.consumeRemainingTokens(alignments, sequences)(AlgorithmStep(0, 4))
      s          <- vector.value.toScalaVector(alignments.s)
      t          <- vector.value.toScalaVector(alignments.t)
    } yield assertEquals((s, t, step), (empty, result, AlgorithmStep(0, 0, 4)))
  }

  test("should consume remaining tokens from left sequence") {
    val left  = AlgorithmSequence(
      Vector(
        AlgorithmToken(Negative1),
        AlgorithmToken(Negative1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1)
      )
    )
    val right = AlgorithmSequence(
      Vector(
        AlgorithmToken(Negative1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Negative1)
      )
    )

    val result = Vector(
      AlgorithmToken(Positive1),
      AlgorithmToken(Positive1),
      AlgorithmToken(Negative1),
      AlgorithmToken(Negative1)
    )
    val empty  = Vector.fill(4)(AlgorithmToken.Empty)

    for {
      service    <- IO(service.value)
      alignments <- service.createEmptyAlignments(4)
      sequences  <- IO(service.Sequences(left, right))
      step       <- service.consumeRemainingTokens(alignments, sequences)(AlgorithmStep(4, 0))
      s          <- vector.value.toScalaVector(alignments.s)
      t          <- vector.value.toScalaVector(alignments.t)
    } yield assertEquals((s, t, step), (result, empty, AlgorithmStep(0, 0, 4)))
  }

  test("should build sequence alignment reversing and taking K elements only") {
    val left  = AlgorithmSequence(
      Vector(
        AlgorithmToken(Positive1),
        AlgorithmToken(Draw),
        AlgorithmToken(Negative1)
      )
    )
    val right = AlgorithmSequence(
      Vector(
        AlgorithmToken(Positive1),
        AlgorithmToken(Positive1),
        AlgorithmToken(Negative1)
      )
    )

    for {
      service    <- IO(service.value)
      alignments <- service.createEmptyAlignments(5)
      _          <- vector.value.set(alignments.s, 0, AlgorithmToken(Negative1))
      _          <- vector.value.set(alignments.t, 0, AlgorithmToken(Negative1))
      _          <- vector.value.set(alignments.s, 1, AlgorithmToken(Draw))
      _          <- vector.value.set(alignments.t, 1, AlgorithmToken(Positive1))
      _          <- vector.value.set(alignments.s, 2, AlgorithmToken(Positive1))
      _          <- vector.value.set(alignments.t, 2, AlgorithmToken(Positive1))
      alignment  <- service.buildSequenceAlignment(alignments)(AlgorithmStep(0, 0, k = 3))
    } yield assertEquals(alignment.left -> alignment.right, left -> right)
  }

  test("should skip left sequence") {
    for {
      service    <- IO(service.value)
      alignments <- service.createEmptyAlignments(1)
      _          <- vector.value.set(alignments.s, 0, AlgorithmToken(Negative1))
      _          <- vector.value.set(alignments.t, 0, AlgorithmToken(Negative1))
      _          <- service.skipLeft(alignments, AlgorithmStep(1, 1))
      left       <- vector.value.get(alignments.s, 0)
      right      <- vector.value.get(alignments.t, 0)
    } yield assertEquals(left -> right, AlgorithmToken.Empty -> AlgorithmToken(Negative1))
  }

  test("should skip right sequence") {
    for {
      service    <- IO(service.value)
      alignments <- service.createEmptyAlignments(1)
      _          <- vector.value.set(alignments.s, 0, AlgorithmToken(Negative1))
      _          <- vector.value.set(alignments.t, 0, AlgorithmToken(Negative1))
      _          <- service.skipRight(alignments, AlgorithmStep(1, 1))
      left       <- vector.value.get(alignments.s, 0)
      right      <- vector.value.get(alignments.t, 0)
    } yield assertEquals(left -> right, AlgorithmToken(Negative1) -> AlgorithmToken.Empty)
  }

  test("should advance left sequence") {
    val left  = AlgorithmSequence(Vector(AlgorithmToken(Draw)))
    val right = AlgorithmSequence(Vector(AlgorithmToken(Negative1)))
    for {
      service    <- IO(service.value)
      alignments <- service.createEmptyAlignments(1)
      sequences  <- IO(service.Sequences(left, right))
      _          <- service.advanceLeft(alignments, sequences, AlgorithmStep(1, 1))
      left       <- vector.value.get(alignments.s, 0)
      right      <- vector.value.get(alignments.t, 0)
    } yield assertEquals(left -> right, AlgorithmToken(Draw) -> AlgorithmToken.Empty)
  }

  test("should advance right sequence") {
    val left  = AlgorithmSequence(Vector(AlgorithmToken(Draw)))
    val right = AlgorithmSequence(Vector(AlgorithmToken(Negative1)))
    for {
      service    <- IO(service.value)
      alignments <- service.createEmptyAlignments(1)
      sequences  <- IO(service.Sequences(left, right))
      _          <- service.advanceRight(alignments, sequences, AlgorithmStep(1, 1))
      left       <- vector.value.get(alignments.s, 0)
      right      <- vector.value.get(alignments.t, 0)
    } yield assertEquals(left -> right, AlgorithmToken.Empty -> AlgorithmToken(Negative1))
  }

  test("should consume up") {
    val left  = AlgorithmSequence(Vector(AlgorithmToken(Draw)))
    val right = AlgorithmSequence(Vector(AlgorithmToken(Negative1)))
    for {
      service    <- IO(service.value)
      alignments <- service.createEmptyAlignments(1)
      sequences  <- IO(service.Sequences(left, right))
      step       <- service.consumeUp(alignments, sequences)(AlgorithmStep(1, 1))
      left       <- vector.value.get(alignments.s, 0)
      right      <- vector.value.get(alignments.t, 0)
    } yield assertEquals((left, right, step), (AlgorithmToken(Draw), AlgorithmToken.Empty, AlgorithmStep(0, 1, 1)))
  }

  test("should consume left") {
    val left  = AlgorithmSequence(Vector(AlgorithmToken(Draw)))
    val right = AlgorithmSequence(Vector(AlgorithmToken(Negative1)))
    for {
      service    <- IO(service.value)
      alignments <- service.createEmptyAlignments(1)
      sequences  <- IO(service.Sequences(left, right))
      step       <- service.consumeLeft(alignments, sequences)(AlgorithmStep(1, 1))
      left       <- vector.value.get(alignments.s, 0)
      right      <- vector.value.get(alignments.t, 0)
    } yield assertEquals((left, right, step), (AlgorithmToken.Empty, AlgorithmToken(Negative1), AlgorithmStep(1, 0, 1)))
  }

  test("should consume upLeft") {
    val left  = AlgorithmSequence(Vector(AlgorithmToken(Draw)))
    val right = AlgorithmSequence(Vector(AlgorithmToken(Negative1)))
    for {
      service    <- IO(service.value)
      alignments <- service.createEmptyAlignments(1)
      sequences  <- IO(service.Sequences(left, right))
      step       <- service.consumeUpLeft(alignments, sequences)(AlgorithmStep(1, 1))
      left       <- vector.value.get(alignments.s, 0)
      right      <- vector.value.get(alignments.t, 0)
    } yield assertEquals((left, right, step), (AlgorithmToken(Draw), AlgorithmToken(Negative1), AlgorithmStep(0, 0, 1)))
  }
}
