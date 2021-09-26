package prediction.data.services

import cats._
import cats.implicits._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._
import prediction.domain.usecases._

sealed abstract class AlgorithmParserService[F[_]: Applicative]
    extends AlgorithmParserUseCase[F, Quotation, AlgorithmToken] {

  private val order = Order[BigDecimal]

  def positive(openingValue: BigDecimal, closingValue: BigDecimal): Alphabet
  def negative(openingValue: BigDecimal, closingValue: BigDecimal): Alphabet

  override def parse(input: Quotation): F[AlgorithmToken] = {
    def decideAlphabet(input: Quotation): Alphabet =
      (input.openingPrice, input.closingPrice) match {
        case (opening, closing) if order.eqv(opening.value, closing.value) => Alphabet.Draw
        case (opening, closing) if order.lt(opening.value, closing.value)  => positive(opening.value, closing.value)
        case (opening, closing) if order.gt(opening.value, closing.value)  => negative(opening.value, closing.value)
      }

    Applicative[F]
      .pure(decideAlphabet(input))
      .map(AlgorithmToken.apply)
  }

  protected def isHalf(newValue: BigDecimal, oldValue: BigDecimal, variance: BigDecimal): Boolean =
    ((newValue - oldValue) / oldValue).abs * 100 <= variance
}

final class AlgorithmParserService3[F[_]: Applicative] extends AlgorithmParserService[F] {
  override def positive(openingValue: BigDecimal, closingValue: BigDecimal): Alphabet = Alphabet.Positive1
  override def negative(openingValue: BigDecimal, closingValue: BigDecimal): Alphabet = Alphabet.Negative1
}

final class AlgorithmParserService5[F[_]: Applicative](variance: BigDecimal) extends AlgorithmParserService[F] {
  override def positive(openingValue: BigDecimal, closingValue: BigDecimal): Alphabet =
    if (isHalf(closingValue, openingValue, variance)) Alphabet.Positive2 else Alphabet.Positive1

  override def negative(openingValue: BigDecimal, closingValue: BigDecimal): Alphabet =
    if (isHalf(openingValue, closingValue, variance)) Alphabet.Negative2 else Alphabet.Negative1
}

final class AlgorithmParserService7[F[_]: Applicative](variance: BigDecimal) extends AlgorithmParserService[F] {
  override def positive(openingValue: BigDecimal, closingValue: BigDecimal): Alphabet =
    if (isHalf(closingValue, openingValue, variance)) Alphabet.Positive2
    else if (isHalf(closingValue, openingValue, variance * 2)) Alphabet.Positive3
    else Alphabet.Positive1

  override def negative(openingValue: BigDecimal, closingValue: BigDecimal): Alphabet =
    if (isHalf(openingValue, closingValue, variance)) Alphabet.Negative2
    else if (isHalf(openingValue, closingValue, variance * 2)) Alphabet.Negative3
    else Alphabet.Negative1
}

final class AlgorithmParserService9[F[_]: Applicative](variance: BigDecimal) extends AlgorithmParserService[F] {
  override def positive(openingValue: BigDecimal, closingValue: BigDecimal): Alphabet =
    if (isHalf(closingValue, openingValue, variance)) Alphabet.Positive2
    else if (isHalf(closingValue, openingValue, variance * 2)) Alphabet.Positive3
    else if (isHalf(closingValue, openingValue, variance * 3)) Alphabet.Positive4
    else Alphabet.Positive1

  override def negative(openingValue: BigDecimal, closingValue: BigDecimal): Alphabet =
    if (isHalf(openingValue, closingValue, variance)) Alphabet.Negative2
    else if (isHalf(openingValue, closingValue, variance * 2)) Alphabet.Negative3
    else if (isHalf(openingValue, closingValue, variance * 3)) Alphabet.Negative4
    else Alphabet.Negative1
}

final class AlgorithmParserService11[F[_]: Applicative](variance: BigDecimal) extends AlgorithmParserService[F] {
  override def positive(openingValue: BigDecimal, closingValue: BigDecimal): Alphabet =
    if (isHalf(closingValue, openingValue, variance)) Alphabet.Positive2
    else if (isHalf(closingValue, openingValue, variance * 2)) Alphabet.Positive3
    else if (isHalf(closingValue, openingValue, variance * 3)) Alphabet.Positive4
    else if (isHalf(closingValue, openingValue, variance * 4)) Alphabet.Positive5
    else Alphabet.Positive1

  override def negative(openingValue: BigDecimal, closingValue: BigDecimal): Alphabet =
    if (isHalf(openingValue, closingValue, variance)) Alphabet.Negative2
    else if (isHalf(openingValue, closingValue, variance * 2)) Alphabet.Negative3
    else if (isHalf(openingValue, closingValue, variance * 3)) Alphabet.Negative4
    else if (isHalf(openingValue, closingValue, variance * 4)) Alphabet.Negative5
    else Alphabet.Negative1
}
