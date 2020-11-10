package prediction.domain

import enumeratum.EnumEntry._
import enumeratum._

sealed abstract class Alphabet extends EnumEntry with Uppercase

object Alphabet extends Enum[Alphabet] with CatsEnum[Alphabet] {
  case object P extends Alphabet
  case object Z extends Alphabet
  case object N extends Alphabet

  override val values: IndexedSeq[Alphabet] = findValues
}
