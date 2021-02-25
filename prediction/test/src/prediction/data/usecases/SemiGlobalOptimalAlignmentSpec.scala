package prediction.data.usecases

import cats._
import cats.implicits._

import cats.effect._

import weaver._
import weaver.scalacheck._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._
import prediction.domain.usecases._
import prediction.infra.services._

import breeze.linalg.{ DenseMatrix, DenseVector }

object SemiGlobalOptimalAlignmentSpec extends SimpleIOSuite with Checkers {

  import SemiGlobalOptimalAlignment._

  test("Building matrix must initialize first row dirty to penalize") {
    for {
      alignment <- optimalAlignment
      M         <- matrix(10, 5)
      S         <- IO(AlgorithmSequence(Vector.fill(4)(AlgorithmToken.Empty)))
      _         <- alignment.initializeForFirstSequenceEmptyTokenPenalty(S, M)
      // format: off
    } yield expect(M(0, 0) === AlgorithmScore(+0)) and expect(M(0, 1) === AlgorithmScore(-2)) and
            expect(M(0, 2) === AlgorithmScore(-4)) and expect(M(0, 3) === AlgorithmScore(-6))
      // format: on
  }

  test("Building matrix must find optimal in the same row as sequence size when both equals") {
    for {
      alignment <- optimalAlignment
      P         <- IO(Vector.fill(4)(AlgorithmToken(Alphabet.Positive)))
      S         <- IO(AlgorithmSequence(P))
      T         <- IO(AlgorithmSequence(P))
      M         <- matrix(S.size + 1, T.size + 1)
      _         <- alignment.calculateBestAlignmentMatrix(S, T, M)
      // format: off
    } yield expect(M(4, 0) === AlgorithmScore(0)) and
            expect(M(4, 1) === AlgorithmScore(2)) and
            expect(M(4, 2) === AlgorithmScore(4)) and
            expect(M(4, 3) === AlgorithmScore(6)) and
            expect(M(4, 4) === AlgorithmScore(8))
      // format: on
  }

  test("Building matrix must find optimal in the same row as sequence size when equals on init") {
    for {
      alignment <- optimalAlignment
      P         <- IO(Vector.fill(4)(AlgorithmToken(Alphabet.Positive)))
      N         <- IO(Vector.fill(2)(AlgorithmToken(Alphabet.Negative)))
      S         <- IO(AlgorithmSequence(P ++ N))
      T         <- IO(AlgorithmSequence(P))
      M         <- matrix(S.size + 1, T.size + 1)
      _         <- alignment.calculateBestAlignmentMatrix(S, T, M)
      // format: off
    } yield expect(M(4, 0) === AlgorithmScore(0)) and
            expect(M(4, 1) === AlgorithmScore(2)) and
            expect(M(4, 2) === AlgorithmScore(4)) and
            expect(M(4, 3) === AlgorithmScore(6)) and
            expect(M(4, 4) === AlgorithmScore(8))
      // format: on
  }

  test("Building matrix must find optimal in the same row as sequence size when equals on tail") {
    for {
      alignment <- optimalAlignment
      P         <- IO(Vector.fill(4)(AlgorithmToken(Alphabet.Positive)))
      N         <- IO(Vector.fill(2)(AlgorithmToken(Alphabet.Negative)))
      S         <- IO(AlgorithmSequence(N ++ P))
      T         <- IO(AlgorithmSequence(P))
      M         <- matrix(S.size + 1, T.size + 1)
      _         <- alignment.calculateBestAlignmentMatrix(S, T, M)
      // format: off
    } yield expect(M(6, 0) === AlgorithmScore(0)) and
            expect(M(6, 1) === AlgorithmScore(2)) and
            expect(M(6, 2) === AlgorithmScore(4)) and
            expect(M(6, 3) === AlgorithmScore(6)) and
            expect(M(6, 4) === AlgorithmScore(8))
      // format: on
  }

  test("Building matrix must allocate matrix with sequences same size and correctly fill it") {
    for {
      alignment <- optimalAlignment
      P         <- IO(Vector.fill(4)(AlgorithmToken(Alphabet.Positive)))
      S         <- IO(AlgorithmSequence(P))
      T         <- IO(AlgorithmSequence(P))
      M         <- alignment.buildMatrix(S, T)
      // format: off
    } yield expect(M(0, 0) === AlgorithmScore(+0)) and
            expect(M(0, 1) === AlgorithmScore(-2)) and
            expect(M(0, 2) === AlgorithmScore(-4)) and
            expect(M(0, 3) === AlgorithmScore(-6)) and
            // --- Build optimal alignment
            expect(M(4, 0) === AlgorithmScore(0)) and
            expect(M(4, 1) === AlgorithmScore(2)) and
            expect(M(4, 2) === AlgorithmScore(4)) and
            expect(M(4, 3) === AlgorithmScore(6)) and
            expect(M(4, 4) === AlgorithmScore(8))
      // format: on
  }

  test("Building matrix must allocate matrix with sequences different size and correctly fill it") {
    for {
      alignment <- optimalAlignment
      V         <- IO(Vector.fill(4)(AlgorithmToken(Alphabet.Positive)))
      P         <- IO(Vector.fill(4)(AlgorithmToken(Alphabet.Positive)))
      N         <- IO(Vector.fill(2)(AlgorithmToken(Alphabet.Negative)))
      S         <- IO(AlgorithmSequence(N ++ P ++ N))
      T         <- IO(AlgorithmSequence(V))
      M         <- alignment.buildMatrix(S, T)
      // format: off
    } yield expect(M.rows === 9) and expect(M.cols === 5) and
            // --- Verify penalty for first sequence gaps
            expect(M(0, 0) === AlgorithmScore(+0)) and
            expect(M(0, 1) === AlgorithmScore(-2)) and
            expect(M(0, 2) === AlgorithmScore(-4)) and
            expect(M(0, 3) === AlgorithmScore(-6)) and
            // --- Build optimal alignment
            expect(M(6, 0) === AlgorithmScore(0)) and
            expect(M(6, 1) === AlgorithmScore(2)) and
            expect(M(6, 2) === AlgorithmScore(4)) and
            expect(M(6, 3) === AlgorithmScore(6)) and
            expect(M(6, 4) === AlgorithmScore(8))
      // format: on
  }

  // -----

  test("Correctly consume up tokens") {
    for {
      alignment <- optimalAlignment
      V         <- IO(AlgorithmSequence(Vector.fill(5)(AlgorithmToken(Alphabet.Positive))))
      S         <- vector(5)
      T         <- vector(5)
      step      <- alignment.consume(V, V)(S, T)(new Step(5, 5, 0), ConsumingDirection.Up)
    } yield expect(step.i === 4) and expect(step.j === 5) and expect(step.k === 1) and
      expect(S(0) === AlgorithmToken(Alphabet.Positive)) and expect(T(0) === AlgorithmToken.Empty)
  }

  test("Correctly consume up left tokens") {
    for {
      alignment <- optimalAlignment
      V         <- IO(AlgorithmSequence(Vector.fill(5)(AlgorithmToken(Alphabet.Positive))))
      S         <- vector(5)
      T         <- vector(5)
      step      <- alignment.consume(V, V)(S, T)(new Step(5, 5, 0), ConsumingDirection.UpLeft)
    } yield expect(step.i === 4) and expect(step.j === 4) and expect(step.k === 1) and
      expect(S(0) === AlgorithmToken(Alphabet.Positive)) and expect(T(0) === AlgorithmToken(Alphabet.Positive))
  }

  test("Correctly consume left tokens") {
    for {
      alignment <- optimalAlignment
      V         <- IO(AlgorithmSequence(Vector.fill(5)(AlgorithmToken(Alphabet.Positive))))
      S         <- vector(5)
      T         <- vector(5)
      step      <- alignment.consume(V, V)(S, T)(new Step(5, 5, 0), ConsumingDirection.Left)
    } yield expect(step.i === 5) and expect(step.j === 4) and expect(step.k === 1) and
      expect(S(0) === AlgorithmToken.Empty) and expect(T(0) === AlgorithmToken(Alphabet.Positive))
  }

  // -----

  test("Search (i, j) coordinates to start the algorithm with penalty for first sequence gaps") {
    for {
      alignment <- optimalAlignment
      M         <- denseMatrix(
                     (0L, 0L, 0L),
                     (0L, 2L, 2L),
                     (0L, 2L, 4L)
                   )
      step      <- alignment.searchStepForFirstSequenceEmptyTokenPenalty(M)
    } yield expect(step.i === 2) and expect(step.j === 2)
  }

  test("Search (i, j) coordinates to start the algorithm only in last column") {
    for {
      alignment <- optimalAlignment
      M         <- denseMatrix(
                     (2L, 2L, 1L),
                     (2L, 2L, 0L),
                     (2L, 2L, 0L)
                   )
      step      <- alignment.searchStepForFirstSequenceEmptyTokenPenalty(M)
    } yield expect(step.i === 0) and expect(step.j === 2)
  }

  test("Search (i, j) coordinates to start the algorithm prefering first occurence") {
    for {
      alignment <- optimalAlignment
      M         <- denseMatrix(
                     (2L, 2L, 1L),
                     (2L, 2L, 2L),
                     (2L, 2L, 2L)
                   )
      step      <- alignment.searchStepForFirstSequenceEmptyTokenPenalty(M)
    } yield expect(step.i === 1) and expect(step.j === 2)
  }

  // -----

  test("Advance sequences until reach the index to start the algorithm") {
    for {
      alignment <- optimalAlignment
      V         <- IO(AlgorithmSequence(Vector.fill(5)(AlgorithmToken(Alphabet.Positive))))
      S         <- vector(5)
      T         <- vector(5)
      step      <- alignment.advanceSequencesFillingTailUntilBeginningRow(V, V)(S, T)(4, 4, 2)
      // format: off
    } yield expect(step.i === 2) and expect(step.j === 4) and
      // --- Verify only the first sequence is consumed
      expect(S(0) === AlgorithmToken(Alphabet.Positive)) and
      expect(S(1) === AlgorithmToken(Alphabet.Positive)) and
      expect(S(2) === AlgorithmToken.Empty) and
      // --- Verify that the second sequence is not changed
      expect(T(0) === AlgorithmToken.Empty) and
      expect(T(1) === AlgorithmToken.Empty) and
      expect(T(2) === AlgorithmToken.Empty)
      // format: on
  }

  // -----

  // test("ALGORITHM") {}

  // -----

  // test("Consuming remaining tokens") {}

  // -----

  test("Fix the token order after sequences aligned") {
    for {
      alignment <- optimalAlignment
      S         <- vector(10)
      T         <- vector(10)
      V         <- IO(AlgorithmSequence(AlgorithmToken.Empty +: Vector.fill(4)(AlgorithmToken(Alphabet.Positive))))
      Z         <- IO(AlgorithmSequence(Vector.fill(5)(AlgorithmToken.Empty)))
      _         <- (0 until 4).toList.traverse_(i => IO(S(i) = AlgorithmToken(Alphabet.Positive)))
      alignment <- alignment.buildAlignment(S, T)(5)
    } yield expect(alignment.first === V) and expect(alignment.second === Z)
  }

  // -----

  private object MockCalculator extends ScoreCalculator[IO] {
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

  private def optimalAlignment: IO[SemiGlobalOptimalAlignment[IO, DenseMatrix, DenseVector]] =
    for {
      matrix    <- BreezeMatrixService.make[IO]
      vector    <- BreezeVectorService.make[IO]
      alignment <- SemiGlobalOptimalAlignment.make(matrix, vector, MockCalculator)
    } yield alignment

  private def matrix(n: Int, m: Int): IO[DenseMatrix[AlgorithmScore]] =
    for {
      service <- BreezeMatrixService.make[IO]
      matrix  <- service.empty(n, m)
    } yield matrix

  private def vector(n: Int): IO[DenseVector[AlgorithmToken]] =
    for {
      service <- BreezeVectorService.make[IO]
      vector  <- service.empty(n)
    } yield vector
}
