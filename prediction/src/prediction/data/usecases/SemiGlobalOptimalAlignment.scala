package prediction.data.usecases

import cats._
import cats.data._
import cats.implicits._

import cats.effect._

import prediction.data.ports.services._
import prediction.domain.entities._
import prediction.domain.entities.algorithm._
import prediction.domain.usecases._

object SemiGlobalOptimalAlignment {
  def make[F[_]: Sync: Parallel, M[_], V[_]](
      matrix: MatrixService[F, M],
      vector: VectorService[F, V],
      score: ScoreCalculator[F]
  ): F[SemiGlobalOptimalAlignment[F, M, V]] =
    Sync[F].delay(new SemiGlobalOptimalAlignment[F, M, V](matrix, vector, score))
}

final class SemiGlobalOptimalAlignment[F[_]: Sync: Parallel, M[_], V[_]] private (
    matrix: MatrixService[F, M],
    vector: VectorService[F, V],
    score: ScoreCalculator[F]
) extends OptimalAlignment[F, M] {

  import AlgorithmToken._
  import Direction._

  override def align(first: AlgorithmSequence, second: AlgorithmSequence, M: M[AlgorithmScore]): F[AlgorithmAlignment] =
    Sync[F].uncancelable {
      for {
        size      <- Sync[F].delay(first.size * second.size)
        S         <- vector.empty(size)
        T         <- vector.empty(size)
        r         <- matrix.rows(M)
        step      <- searchStepForFirstSequenceEmptyTokenPenalty(M)
        step      <- advanceSequencesFillingTailUntilBeginningRow(first, second)(S, T)(RowIndex(r - 1), step.j, step.i)
        step      <- calculateBestAlignment(first, second)(S, T, M)(step)
        step      <- consumeRemainingTokens(first, second)(S, T)(step)
        alignment <- buildAlignment(S, T)(step.k.value)
      } yield alignment
    }

  override def buildMatrix(first: AlgorithmSequence, second: AlgorithmSequence): F[M[AlgorithmScore]] =
    Sync[F].uncancelable {
      for {
        M <- matrix.empty(first.size + 1, second.size + 1)
        _ <- matrix.set(M, RowIndex.Empty, ColumnIndex.Empty, AlgorithmScore.Empty)
        _ <- initializeForFirstSequenceEmptyTokenPenalty(second, M)
        _ <- calculateBestAlignmentMatrix(first, second, M)
      } yield M
    }

  override def coordinatesMaxValue(M: M[AlgorithmScore]): F[(RowIndex, ColumnIndex)] =
    searchStepForFirstSequenceEmptyTokenPenalty(M).map(step => (step.i, step.j))

  // -----

  private def sum(f: F[AlgorithmScore], g: F[AlgorithmScore]): F[AlgorithmScore] =
    (f, g).parMapN { case (f, g) => f + g }

  // -----

  def consume(first: AlgorithmSequence, second: AlgorithmSequence)(S: V[AlgorithmToken], T: V[AlgorithmToken])(
      step: Step,
      direction: Direction
  ): F[Step] = {
    import step._

    def consumeUp: F[Step] =
      (vector.set(S, k, first(i)), vector.set(T, k, Empty)).parTupled.as(step.up)

    def consumeUpLeft: F[Step] =
      (vector.set(S, k, first(i)), vector.set(T, k, second(j))).parTupled.as(step.upLeft)

    def consumeLeft: F[Step] =
      (vector.set(S, k, Empty), vector.set(T, k, second(j))).parTupled.as(step.left)

    direction match {
      case Up     => consumeUp
      case UpLeft => consumeUpLeft
      case Left   => consumeLeft
    }
  }

  def searchStepForFirstSequenceEmptyTokenPenalty(M: M[AlgorithmScore]): F[Step] =
    for {
      cols   <- matrix.cols(M)
      (i, j) <- matrix.coordinatesMaxColumn(M, ColumnIndex(cols - 1))
    } yield new Step(i, j, SequenceIndex.Empty)

  def advanceSequencesFillingTailUntilBeginningRow(first: AlgorithmSequence, second: AlgorithmSequence)(
      S: V[AlgorithmToken],
      T: V[AlgorithmToken]
  )(startRow: RowIndex, column: ColumnIndex, targetRow: RowIndex): F[Step] = {
    def go(step: Step): F[Step] =
      if (step.i === targetRow) Applicative[F].pure(step)
      else consume(first, second)(S, T)(step, Up) >>= go

    go(new Step(startRow, column, SequenceIndex.Empty))
  }

  def calculateBestAlignment(first: AlgorithmSequence, second: AlgorithmSequence)(
      S: V[AlgorithmToken],
      T: V[AlgorithmToken],
      M: M[AlgorithmScore]
  )(step: Step): F[Step] = {
    def go(step: Step): F[Step] = {
      import step._

      def calculate: F[Step] = {
        // format: off
        val up     = sum(matrix.get(M, i - 1, j    ), score.calculate(first(i), Empty    ))
        val upLeft = sum(matrix.get(M, i - 1, j - 1), score.calculate(first(i), second(j)))
        val left   = sum(matrix.get(M, i    , j - 1), score.calculate(Empty   , second(j)))
        val value  = matrix.get(M, i, j)

        (value, up, upLeft, left).parMapN {
          case (target, _, calculated, _) if target === calculated =>
            consume(first, second)(S, T)(step, UpLeft) >>= go
          case (target, _, _, calculated) if target === calculated =>
            consume(first, second)(S, T)(step, Left)   >>= go
          case (target, calculated, _, _) if target === calculated =>
            consume(first, second)(S, T)(step, Up)     >>= go
        }.flatten
        // format: on
      }

      if (i =!= RowIndex.Empty && j =!= ColumnIndex.Empty) calculate
      else Sync[F].delay(step)
    }

    go(step)
  }

  def consumeRemainingTokens(first: AlgorithmSequence, second: AlgorithmSequence)(
      S: V[AlgorithmToken],
      T: V[AlgorithmToken]
  )(step: Step): F[Step] = {
    def go(step: Step): F[Step] = {
      import step._

      // format: off
      (i, j) match {
        case (0, 0) => Sync[F].delay(step)
        case (_, 0) => consume(first, second)(S, T)(step, Up)   >>= go
        case (0, _) => consume(first, second)(S, T)(step, Left) >>= go
      }
      // format: on
    }

    go(step)
  }

  def buildAlignment(S: V[AlgorithmToken], T: V[AlgorithmToken])(size: Int): F[AlgorithmAlignment] = {
    def reverseApply(V: V[AlgorithmToken]): F[AlgorithmSequence] =
      vector.toVector(V).map(_.take(size)).map(_.reverse).map(AlgorithmSequence.apply)

    (reverseApply(S), reverseApply(T))
      .parMapN(AlgorithmAlignment.apply)
  }

  // -----

  def initializeForFirstSequenceEmptyTokenPenalty(second: AlgorithmSequence, M: M[AlgorithmScore]): F[Unit] =
    (1 to second.size).toList.parTraverse_ { k =>
      for {
        s <- score.calculate(Empty, second(k))
        _ <- matrix.set(M, RowIndex.Empty, ColumnIndex(k), s * k)
      } yield ()
    }

  def calculateBestAlignmentMatrix(first: AlgorithmSequence, second: AlgorithmSequence, M: M[AlgorithmScore]): F[Unit] =
    (1 to first.size).toList.map(RowIndex.apply).traverse_ { i =>
      (1 to second.size).toList.map(ColumnIndex.apply).traverse_ { j =>
        // format: off
        val up     = sum(matrix.get(M, i - 1, j    ), score.calculate(first(i), Empty    ))
        val upLeft = sum(matrix.get(M, i - 1, j - 1), score.calculate(first(i), second(j)))
        val left   = sum(matrix.get(M, i    , j - 1), score.calculate(Empty   , second(j)))
        // format: on

        NonEmptyList.of(up, upLeft, left).parSequence.map(_.maximum) >>= (matrix.set(M, i, j, _))
      }
    }
}
