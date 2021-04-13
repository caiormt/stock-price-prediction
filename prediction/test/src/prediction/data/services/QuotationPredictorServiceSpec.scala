package prediction.data.services

import cats._
import cats.implicits._

import cats.effect._

import munit._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._

import prediction.adapter.breeze.algorithm._
import prediction.adapter.services._

import breeze.linalg._

import java.{ time => jt }

final class QuotationPredictorServiceSpec extends CatsEffectSuite {

  private val service = Eval.later {
    val algorithmParser  = new AlgorithmParserService[IO]
    val score            = new ScoreCalculatorService[IO]
    val matrix           = new BreezeMatrixAdapter[IO, AlgorithmScore]
    val alignment        = new AlignmentFinderService[IO, DenseMatrix](matrix)
    val optimalAlignment = new OptimalAlignmentBuilderService[IO, DenseMatrix](matrix, score)
    new QuotationPredictorService[IO, DenseMatrix](algorithmParser, optimalAlignment, alignment)
  }

  test("should predict next value") {
    val positive  =
      Quotation(
        QuotationExchangeDate(jt.LocalDateTime.now),
        QuotationNegotiationCode("BOVA11"),
        QuotationOpeningPrice(5),
        QuotationClosingPrice(10)
      )
    val actual    =
      scala.collection.immutable.Vector.fill(3) {
        Quotation(
          QuotationExchangeDate(jt.LocalDateTime.now),
          QuotationNegotiationCode("BOVA11"),
          QuotationOpeningPrice(5),
          QuotationClosingPrice(5)
        )
      }
    val reference = actual :+ positive

    val result = service.value.predict(actual, reference)

    assertIO(result, Alphabet.Positive.some)
  }

  test("should return none when does not have prediction") {
    val actual    =
      scala.collection.immutable.Vector.fill(3) {
        Quotation(
          QuotationExchangeDate(jt.LocalDateTime.now),
          QuotationNegotiationCode("BOVA11"),
          QuotationOpeningPrice(5),
          QuotationClosingPrice(5)
        )
      }
    val reference = actual

    val result = service.value.predict(actual, reference)

    assertIO(result, none)
  }
}
