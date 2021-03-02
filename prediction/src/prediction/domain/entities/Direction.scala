package prediction.domain.entities

import enumeratum.values._

sealed abstract class Direction(override val value: Char) extends CharEnumEntry

object Direction extends CharEnum[Direction] with CatsValueEnum[Char, Direction] {
  // format: off
  case object Up     extends Direction('↑')
  case object UpLeft extends Direction('↖')
  case object Left   extends Direction('←')
  // format: on

  override val values: IndexedSeq[Direction] = findValues
}
