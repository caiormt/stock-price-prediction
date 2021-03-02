package prediction.data.usecases

import cats.implicits._

import cats.effect._

import weaver._
import weaver.scalacheck._

import prediction.domain.entities.algorithm._
import prediction.domain.usecases._
import prediction.infra.services._

import breeze.linalg._

object SemiGlobalSimilarityCalculatorSpec extends SimpleIOSuite with Checkers {

  private def similarityCalculator: IO[SimilarityCalculator[IO, DenseMatrix]] =
    BreezeMatrixService.make[IO] >>= (SemiGlobalSimilarityCalculator.make(_))

  test("Maximum score in last column") {
    for {
      matrix     <- denseMatrix(
                      (1L, 6L, 3L),
                      (4L, 2L, 2L),
                      (1L, 1L, 1L)
                    )
      similarity <- similarityCalculator
      score      <- similarity.calculate(matrix)
    } yield expect(score === AlgorithmScore(3))
  }

  test("Maximum score in last row") {
    for {
      matrix     <- denseMatrix(
                      (1L, 6L, 0L),
                      (4L, 2L, 2L),
                      (3L, 1L, 1L)
                    )
      similarity <- similarityCalculator
      score      <- similarity.calculate(matrix)
    } yield expect(score === AlgorithmScore(3))
  }

  test("Maximum score in last row or column") {
    for {
      matrix     <- denseMatrix(
                      (1L, 6L, 0L),
                      (4L, 2L, 0L),
                      (0L, 0L, 0L)
                    )
      similarity <- similarityCalculator
      score      <- similarity.calculate(matrix)
    } yield expect(score === AlgorithmScore(0))
  }
}
