package prediction.domain.entities

import cats._
import cats.derived._

import io.estatico.newtype.macros._

object algorithm {
  @newtype case class AlgorithmToken(value: Alphabet)
  object AlgorithmToken {
    val Empty: AlgorithmToken = AlgorithmToken(Alphabet.Empty)

    implicit val eqForAlgorithmToken: Eq[AlgorithmToken]     = deriving
    implicit val showForAlgorithmToken: Show[AlgorithmToken] = deriving
  }

  @newtype case class AlgorithmSequence(value: Vector[AlgorithmToken])
  object AlgorithmSequence {
    implicit val eqForAlgorithmSequence: Eq[AlgorithmSequence]     = deriving
    implicit val showForAlgorithmSequence: Show[AlgorithmSequence] = deriving

    implicit final class AlgorithmSequenceOps(private val sequence: AlgorithmSequence) extends AnyVal {
      def apply(index: Int): AlgorithmToken = sequence.value(index - 1)
      def size: Int                         = sequence.value.size
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
}
