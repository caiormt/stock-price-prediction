package prediction.domain.entities

import cats._
import cats.derived._
import cats.implicits._

import io.estatico.newtype.macros._

object algorithm {
  @newtype case class AlgorithmToken(value: Alphabet)
  object AlgorithmToken {
    val Empty: AlgorithmToken = AlgorithmToken(Alphabet.Empty)

    implicit val eqForAlgorithmToken: Eq[AlgorithmToken]     = deriving
    implicit val showForAlgorithmToken: Show[AlgorithmToken] =
      Show.show[AlgorithmToken](_.value.value.show)
  }

  @newtype case class AlgorithmSequence(value: Vector[AlgorithmToken])
  object AlgorithmSequence {
    implicit val eqForAlgorithmSequence: Eq[AlgorithmSequence]     = deriving
    implicit val showForAlgorithmSequence: Show[AlgorithmSequence] =
      Show.show[AlgorithmSequence](_.value.foldMap(_.show))

    implicit final class AlgorithmSequenceOps(private val sequence: AlgorithmSequence) extends AnyVal {
      def apply(index: Index): AlgorithmToken = sequence.value(index.value - 1)
      def apply(index: Int): AlgorithmToken   = sequence.value(index - 1)
      def size: Int                           = sequence.value.size
    }
  }

  @newtype case class AlgorithmScore(value: Long)
  object AlgorithmScore {
    val Empty: AlgorithmScore = AlgorithmScore(0L)

    implicit val orderForAlgorithmScore: Order[AlgorithmScore] = deriving
    implicit val showForAlgorithmScore: Show[AlgorithmScore]   = deriving

    implicit final class AlgorithmScoreOps(private val score: AlgorithmScore) extends AnyVal {
      def +(that: AlgorithmScore): AlgorithmScore = AlgorithmScore(score.value + that.value)
      def *(that: AlgorithmScore): AlgorithmScore = AlgorithmScore(score.value * that.value)
      def *(that: Int): AlgorithmScore            = AlgorithmScore(score.value * that.toLong)
      def *(that: Long): AlgorithmScore           = AlgorithmScore(score.value * that)
    }
  }

  final case class AlgorithmAlignment(first: AlgorithmSequence, second: AlgorithmSequence)
  object AlgorithmAlignment {
    implicit val eqForAlgorithmAlignment: Eq[AlgorithmAlignment]     = semiauto.eq
    implicit val showForAlgorithmAlignment: Show[AlgorithmAlignment] = semiauto.show
  }

  // -----

  @newtype case class Index(value: Int)
  object Index {
    implicit val eqForIndex: Eq[Index]     = deriving
    implicit val showForIndex: Show[Index] = deriving
  }

  @newtype case class RowIndex(value: Int)
  object RowIndex {
    val Empty: RowIndex = RowIndex(0)

    implicit val eqForRowIndex: Eq[RowIndex]     = deriving
    implicit val showForRowIndex: Show[RowIndex] = deriving

    implicit final class RowIndexOps(private val row: RowIndex) extends AnyVal {
      def -(value: Int): RowIndex = RowIndex(row.value - value)
      def +(value: Int): RowIndex = RowIndex(row.value + value)
    }
  }

  @newtype case class ColumnIndex(value: Int)
  object ColumnIndex {
    val Empty: ColumnIndex = ColumnIndex(0)

    implicit val eqForColumnIndex: Eq[ColumnIndex]     = deriving
    implicit val showForColumnIndex: Show[ColumnIndex] = deriving

    implicit final class ColumnIndexOps(private val column: ColumnIndex) extends AnyVal {
      def -(value: Int): ColumnIndex = ColumnIndex(column.value - value)
    }
  }

  @newtype case class SequenceIndex(value: Int)
  object SequenceIndex {
    val Empty: SequenceIndex = SequenceIndex(0)

    implicit val eqForSequenceIndex: Eq[SequenceIndex]     = deriving
    implicit val showForSequenceIndex: Show[SequenceIndex] = deriving

    implicit final class SequenceIndexOps(private val sequence: SequenceIndex) extends AnyVal {
      def +(value: Int): SequenceIndex = SequenceIndex(sequence.value + value)
    }
  }

  final case class Step(i: RowIndex, j: ColumnIndex, k: SequenceIndex)
  object Step {
    implicit val eqForStep: Eq[Step]     = semiauto.eq
    implicit val showForStep: Show[Step] = semiauto.show

    implicit final class StepOps(private val step: Step) extends AnyVal {
      import step._
      // format: off
      def up: Step     = Step(i - 1, j    , k + 1)
      def upLeft: Step = Step(i - 1, j - 1, k + 1)
      def left: Step   = Step(i    , j - 1, k + 1)
      // format: on
    }
  }

  // -----

  sealed abstract class AlgorithmError extends RuntimeException
  final case object NotEnoughDataToPredict extends AlgorithmError

  // -----

  implicit final class VectorIndexedOps[A](private val vector: Vector[A]) extends AnyVal {
    def at(index: Index): A = vector(index.value - 1)
  }

  implicit def RowIndexToIndexOps(rowIndex: RowIndex): Index =
    Index(rowIndex.value)

  implicit def ColumnIndexToIndexOps(columnIndex: ColumnIndex): Index =
    Index(columnIndex.value)

  implicit def SequenceIndexToIndexOps(sequenceIndex: SequenceIndex): Index =
    Index(sequenceIndex.value)
}
