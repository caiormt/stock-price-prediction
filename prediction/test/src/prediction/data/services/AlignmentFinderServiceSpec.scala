package prediction.data.services

import cats._

import cats.effect._

import munit._

import prediction.domain.entities.algorithm._

import prediction.adapter.breeze.algorithm._
import prediction.adapter.services._

import breeze.linalg._

final class AlignmentFinderServiceSpec extends CatsEffectSuite {

  private val service = Eval.later {
    val matrix = new BreezeMatrixAdapter[IO, AlgorithmScore]
    new AlignmentFinderService[IO, DenseMatrix](matrix)
  }

  test("should ask for maximum value from last column") {
    val data = DenseMatrix(
      (AlgorithmScore(+0), AlgorithmScore(+0), AlgorithmScore(15), AlgorithmScore(+2)),
      (AlgorithmScore(12), AlgorithmScore(+0), AlgorithmScore(+0), AlgorithmScore(+2)),
      (AlgorithmScore(+0), AlgorithmScore(10), AlgorithmScore(+0), AlgorithmScore(+6)),
      (AlgorithmScore(+0), AlgorithmScore(+0), AlgorithmScore(+8), AlgorithmScore(+4))
    )

    val result = service.value.findCoordinates(data)

    assertIO(result, (2, 3))
  }
}
