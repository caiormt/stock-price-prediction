package prediction.adapter.services

import cats._
import cats.implicits._

import cats.effect._

import munit._

import breeze.linalg._

final class BreezeMatrixAdapterSpec extends CatsEffectSuite {

  private val service = Eval.later(new BreezeMatrixAdapter[IO, Long])

  test("should create empty matrix") {
    val matrix = service.value.empty(5, 4).unsafeRunSync()

    assertEquals(matrix.rows, 5)
    assertEquals(matrix.cols, 4)
    assert(matrix.valuesIterator.forall(_ === 0), "all values are not empty")
  }

  test("should set value") {
    val matrix = DenseMatrix.zeros[Long](5, 5)

    service.value.set(matrix, 1, 2, 5).unsafeRunSync()

    assertEquals(matrix(1, 2), 5L)
  }

  test("should get value") {
    val matrix = DenseMatrix.zeros[Long](5, 5)
    matrix(1, 2) = 5

    val result = service.value.get(matrix, 1, 2)

    assertIO(result, 5L)
  }

  test("should get matrix size") {
    val matrix = DenseMatrix.zeros[Long](5, 4)

    val result = service.value.size(matrix)

    assertIO(result, (5, 4))
  }

  test("should get coordinates for maximum value") {
    val matrix = DenseMatrix.zeros[Long](5, 4)
    matrix(2, 3) = 5

    val result = service.value.maximumBy(matrix, _ => true)

    assertIO(result, (2, 3))
  }
}
