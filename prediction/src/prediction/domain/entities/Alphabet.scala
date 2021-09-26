package prediction.domain.entities

import cats._

import enumeratum.values._

sealed abstract class Alphabet(override val value: Char, val familly: String) extends CharEnumEntry

object Alphabet extends CharEnum[Alphabet] with CatsValueEnum[Char, Alphabet] {
  // format: off
  case object Empty      extends Alphabet('-', "Empty")
  case object Positive1  extends Alphabet('P', "Positive")
  case object Positive2  extends Alphabet('O', "Positive")
  case object Positive3  extends Alphabet('I', "Positive")
  case object Positive4  extends Alphabet('U', "Positive")
  case object Positive5  extends Alphabet('Y', "Positive")
  case object Draw       extends Alphabet('Z', "Draw")
  case object Negative1  extends Alphabet('X', "Negative")
  case object Negative2  extends Alphabet('C', "Negative")
  case object Negative3  extends Alphabet('V', "Negative")
  case object Negative4  extends Alphabet('B', "Negative")
  case object Negative5  extends Alphabet('N', "Negative")
  // format: on

  override val values: IndexedSeq[Alphabet] = findValues

  implicit override val eqInstance: Eq[Alphabet] = Eq.by(_.familly)
}
