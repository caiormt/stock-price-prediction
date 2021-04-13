package prediction.domain.entities

import cats._
import cats.derived._
import cats.implicits._

import io.estatico.newtype.macros._

object algorithm {
  @newtype case class AlgorithmToken(value: Alphabet)
  object AlgorithmToken {
    val Empty: AlgorithmToken = AlgorithmToken(Alphabet.Empty)

    implicit val eqForAlgorithmToken: Eq[AlgorithmToken] =
      deriving

    implicit val showForAlgorithmToken: Show[AlgorithmToken] =
      Show.show[AlgorithmToken](_.value.value.show)
  }

  @newtype case class AlgorithmSequence(value: Vector[AlgorithmToken])
  object AlgorithmSequence {
    implicit val eqForAlgorithmSequence: Eq[AlgorithmSequence] =
      deriving

    implicit val showForAlgorithmSequence: Show[AlgorithmSequence] =
      Show.show[AlgorithmSequence](_.value.map(_.show).mkString_("[", "", "]"))

    implicit final class AlgorithmSequenceOps(private val sequence: AlgorithmSequence) extends AnyVal {
      def size: Int =
        sequence.value.size

      def at(index: Int): AlgorithmToken =
        sequence.value(index - 1)

      def liftAt(index: Int): Option[AlgorithmToken] =
        sequence.value.lift(index - 1)
    }
  }

  final case class AlgorithmAlignment(left: AlgorithmSequence, right: AlgorithmSequence)
  object AlgorithmAlignment {
    implicit val eqForAlgorithmAlignment: Eq[AlgorithmAlignment] =
      semiauto.eq

    implicit val showForAlgorithmAlignment: Show[AlgorithmAlignment] =
      Show.show[AlgorithmAlignment](alignment => show"${alignment.left}\n${alignment.right}")
  }

  @newtype case class AlgorithmScore(value: Long)
  object AlgorithmScore {
    val Empty: AlgorithmScore = AlgorithmScore(0L)
    val One: AlgorithmScore   = AlgorithmScore(1L)

    implicit val orderForAlgorithmScore: Order[AlgorithmScore] =
      deriving

    implicit val showForAlgorithmScore: Show[AlgorithmScore] =
      deriving

    implicit final class AlgorithmScoreOps(private val score: AlgorithmScore) extends AnyVal {
      def *(that: Int): AlgorithmScore =
        AlgorithmScore(score.value * that)

      def +(that: AlgorithmScore): AlgorithmScore =
        AlgorithmScore(score.value + that.value)
    }
  }

  /**
    * Represents the snapshot at each iteration during sequence alignment.
    *
    * @param i index of first sequence to consume
    * @param j index of second sequence to consume
    * @param k index to attribute the token at `i` index from first sequence or `j` index from second sequence.
    */
  final case class AlgorithmStep(i: Int, j: Int, k: Int)
  object AlgorithmStep {
    def apply(i: Int, j: Int): AlgorithmStep =
      AlgorithmStep(i, j, k = 0)

    implicit val eqForAlgorithmStep: Eq[AlgorithmStep] =
      semiauto.eq

    implicit val showForAlgorithmStep: Show[AlgorithmStep] =
      semiauto.show

    implicit final class AlgorithmStepOps(private val algorithmStep: AlgorithmStep) extends AnyVal {
      import algorithmStep._
      // format: off
      def up: AlgorithmStep     = AlgorithmStep(i - 1, j    , k + 1)
      def upLeft: AlgorithmStep = AlgorithmStep(i - 1, j - 1, k + 1)
      def left: AlgorithmStep   = AlgorithmStep(i    , j - 1, k + 1)
      // format: on
    }
  }
}
