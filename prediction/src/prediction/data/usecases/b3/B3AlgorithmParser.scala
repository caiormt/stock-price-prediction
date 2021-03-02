package prediction.data.usecases

import cats._
import cats.implicits._

import cats.effect._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._
import prediction.domain.usecases._

object B3AlgorithmParser {
  def make[F[_]: Sync]: F[B3AlgorithmParser[F]] =
    Sync[F].delay(new B3AlgorithmParser[F])
}

final class B3AlgorithmParser[F[_]: cats.ApplicativeThrow] extends AlgorithmParser[F] {

  private val order = Order[BigDecimal]

  override def parseQuotation(quotation: Quotation): F[AlgorithmToken] = {
    def decideAlphabet(quotation: Quotation): Alphabet =
      (quotation.openingPrice, quotation.closingPrice) match {
        case (opening, closing) if order.eqv(opening.value, closing.value) => Alphabet.Draw
        case (opening, closing) if order.lt(opening.value, closing.value)  => Alphabet.Positive
        case (opening, closing) if order.gt(opening.value, closing.value)  => Alphabet.Negative
      }

    Applicative[F]
      .pure(decideAlphabet(quotation))
      .map(AlgorithmToken.apply)
  }
}
