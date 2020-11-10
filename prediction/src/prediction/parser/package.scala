package prediction

import atto.Atto._
import atto._

import java.{ time => jt }

import scala.math.BigDecimal.RoundingMode

package object parser {
  val dateFormat: jt.format.DateTimeFormatter =
    jt.format.DateTimeFormatter.ofPattern("yyyyMMdd")

  val date: Parser[jt.LocalDate] =
    count(8, digit).map(_.mkString).flatMap { str =>
      try ok(jt.LocalDate.parse(str, dateFormat))
      catch { case e: jt.format.DateTimeParseException => err(e.toString()) }
    }

  def stringTrim(n: Int): Parser[String] =
    take(n).map(_.trim)

  def numeric(n: Int): Parser[BigDecimal] =
    take(n).map(BigDecimal.apply)

  def numericV11(n: Int): Parser[BigDecimal] =
    numeric(n).map(_.setScale(2, RoundingMode.HALF_EVEN)).map(_ / 100)
}
