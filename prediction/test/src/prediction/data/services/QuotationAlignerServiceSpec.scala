package prediction.data.services

import cats._
import cats.implicits._

import cats.effect._

import munit._

import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._

import prediction.adapter.breeze.algorithm._
import prediction.adapter.services._

import breeze.linalg._

import java.{ time => jt }

final class QuotationAlignerServiceSpec extends CatsEffectSuite {

  private val service = Eval.later {
    val parser    = new AlgorithmParserService[IO]
    val matrix    = new BreezeMatrixAdapter[IO, AlgorithmScore]
    val vector    = new BreezeVectorAdapter[IO, AlgorithmToken]
    val score     = new ScoreCalculatorService[IO]
    val alignment = new AlignmentFinderService[IO, DenseMatrix](matrix)
    val builder   = new OptimalAlignmentBuilderService[IO, DenseMatrix](matrix, score)
    val aligner   = new OptimalAlignmentAlignerService(matrix, vector, score, alignment, builder)
    new QuotationAlignerService(parser, aligner)
  }

  test("should align quotations") {
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

    val result = service.value.align(actual, reference)

    assertIO(result.map(_.show), "[ZZZP]\n[ZZZ-]")
  }
}
