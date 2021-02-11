package prediction.domain.entities

import enumeratum.values._

sealed abstract class Alphabet(override val value: Char) extends CharEnumEntry

object Alphabet extends CharEnum[Alphabet] with CatsValueEnum[Char, Alphabet] {
  // format: off
  case object Empty    extends Alphabet('-')
  case object Positive extends Alphabet('P')
  case object Draw     extends Alphabet('Z')
  case object Negative extends Alphabet('N')
  // format: on

  override val values: IndexedSeq[Alphabet] = findValues
}
