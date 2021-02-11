package prediction.data.usecases

import cats.implicits._

import cats.effect._

import weaver._
import weaver.scalacheck._

import prediction.domain.entities.algorithm._
import prediction.infra.services._

import breeze.linalg._

object SemiGlobalSimilarityCalculatorSpec extends SimpleIOSuite with Checkers {
  test("Maximum score in last row or column") {
    for {
      matrix     <- DenseMatrix(
                      (1L, 6L, 3L),
                      (4L, 2L, 2L),
                      (1L, 1L, 1L)
                    ).map(AlgorithmScore.apply).pure[IO]
      service    <- BreezeMatrixService.make[IO]
      similarity <- SemiGlobalSimilarityCalculator.make(service)
      score      <- similarity.calculate(matrix)
    } yield expect(score === AlgorithmScore(3))
  }
}
