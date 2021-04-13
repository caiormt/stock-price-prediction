package prediction.data.services

import cats._
import cats.implicits._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._
import prediction.domain.usecases._

final class AlgorithmParserService[F[_]: Applicative] extends AlgorithmParserUseCase[F, Quotation, AlgorithmToken] {

  private val order = Order[BigDecimal]

  override def parse(input: Quotation): F[AlgorithmToken] = {
    def decideAlphabet(input: Quotation): Alphabet =
      (input.openingPrice, input.closingPrice) match {
        case (opening, closing) if order.eqv(opening.value, closing.value) => Alphabet.Draw
        case (opening, closing) if order.lt(opening.value, closing.value)  => Alphabet.Positive
        case (opening, closing) if order.gt(opening.value, closing.value)  => Alphabet.Negative
      }

    Applicative[F]
      .pure(decideAlphabet(input))
      .map(AlgorithmToken.apply)
  }
}
