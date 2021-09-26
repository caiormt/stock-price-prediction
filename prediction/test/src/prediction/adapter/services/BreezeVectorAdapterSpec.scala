package prediction.adapter.services

import cats._
import cats.implicits._

import cats.effect._

import munit._

import breeze.linalg._
import natchez.Trace.Implicits._

final class BreezeVectorAdapterSpec extends CatsEffectSuite {

  private val service = Eval.later(new BreezeVectorAdapter[IO, Long])

  test("should create empty vector") {
    val vector = service.value.empty(5).unsafeRunSync()

    assertEquals(vector.length, 5)
    assert(vector.valuesIterator.forall(_ === 0), "all values are not empty")
  }

  test("should set value") {
    val vector = DenseVector.zeros[Long](5)

    service.value.set(vector, 1, 5).unsafeRunSync()

    assertEquals(vector(1), 5L)
  }

  test("should get value") {
    val vector = DenseVector.zeros[Long](5)
    vector(1) = 5

    val result = service.value.get(vector, 1)

    assertIO(result, 5L)
  }

  test("should transform into scala's vector") {
    val vector = DenseVector.zeros[Long](5)

    val result = service.value.toScalaVector(vector)

    assertIO(result, scala.collection.immutable.Vector.fill(5)(0L))
  }
}
