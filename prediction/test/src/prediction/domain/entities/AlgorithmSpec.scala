package prediction.domain.entities

import cats.implicits._

import munit._

import prediction.domain.entities.Alphabet._
import prediction.domain.entities.algorithm._

final class AlgorithmSpec extends CatsEffectSuite {

  // ----- AlgorithmToken

  test("AlgorithmToken.Empty should be AlgorithmToken(Empty)") {
    assertEquals(AlgorithmToken.Empty, AlgorithmToken(Empty))
  }

  test("AlgorithmToken.eq should work correctly") {
    assert(AlgorithmToken(Empty) === AlgorithmToken(Empty))
  }

  test("AlgorithmToken.show should use Alphabet.show") {
    assertEquals(AlgorithmToken(Empty).show, Empty.value.show)
    assertEquals(AlgorithmToken(Draw).show, Draw.value.show)
    assertEquals(AlgorithmToken(Positive).show, Positive.value.show)
    assertEquals(AlgorithmToken(Negative).show, Negative.value.show)
  }

  // ----- AlgorithmSequence

  test("AlgorithmSequence.eq should work correctly") {
    assert(AlgorithmSequence(Vector(AlgorithmToken.Empty)) === AlgorithmSequence(Vector(AlgorithmToken.Empty)))
  }

  test("AlgorithmSequence.show should use AlgorithmToken.show") {
    val sequence = AlgorithmSequence(
      Vector(
        AlgorithmToken(Draw),
        AlgorithmToken(Positive),
        AlgorithmToken(Negative)
      )
    )

    val inner = sequence.value.map(_.show).mkString
    assertEquals(sequence.show, s"[$inner]")
  }

  test("AlgorithmSequence.size should return sequence size") {
    val sequence = AlgorithmSequence(
      Vector(
        AlgorithmToken(Draw),
        AlgorithmToken(Positive),
        AlgorithmToken(Negative)
      )
    )
    assertEquals(sequence.size, 3)
  }

  test("AlgorithmSequence.at(index) should access starting index at 1") {
    val sequence = AlgorithmSequence(
      Vector(
        AlgorithmToken(Draw),
        AlgorithmToken(Positive),
        AlgorithmToken(Negative)
      )
    )
    intercept[IndexOutOfBoundsException](sequence.at(0))
    assertEquals(sequence.at(1), AlgorithmToken(Draw))
    assertEquals(sequence.at(2), AlgorithmToken(Positive))
    assertEquals(sequence.at(3), AlgorithmToken(Negative))
    intercept[IndexOutOfBoundsException](sequence.at(4))
  }

  test("AlgorithmSequence.liftAt(index) should access starting index at 1") {
    val sequence = AlgorithmSequence(
      Vector(
        AlgorithmToken(Draw),
        AlgorithmToken(Positive),
        AlgorithmToken(Negative)
      )
    )
    assertEquals(sequence.liftAt(0), none)
    assertEquals(sequence.liftAt(1), AlgorithmToken(Draw).some)
    assertEquals(sequence.liftAt(2), AlgorithmToken(Positive).some)
    assertEquals(sequence.liftAt(3), AlgorithmToken(Negative).some)
    assertEquals(sequence.liftAt(4), none)
  }

  // ----- AlgorithmAlignment

  test("AlgorithmAlignment.eq should work correctly") {
    val left  = AlgorithmSequence(Vector(AlgorithmToken.Empty))
    val right = AlgorithmSequence(Vector(AlgorithmToken(Draw)))
    assert(AlgorithmAlignment(left, right) === AlgorithmAlignment(left, right))
  }

  test("AlgorithmAlignment.show should use AlgorithmSequence.show") {
    val left      = AlgorithmSequence(Vector(AlgorithmToken.Empty))
    val right     = AlgorithmSequence(Vector(AlgorithmToken(Draw)))
    val alignment = AlgorithmAlignment(left, right)
    assertEquals(alignment.show, show"$left\n$right")
  }

  // ----- AlgorithmScore

  test("AlgorithmScore.eq should work correctly") {
    assert(AlgorithmScore(5L) === AlgorithmScore(5L))
  }

  test("AlgorithmScore.show should work correctly") {
    val score = AlgorithmScore(5L)
    assertEquals(score.show, "5")
  }

  test("AlgorithmScore.Empty should be AlgorithmScore(0)") {
    assertEquals(AlgorithmScore.Empty, AlgorithmScore(0L))
  }

  test("AlgorithmScore.One should be AlgorithmScore(1)") {
    assertEquals(AlgorithmScore.One, AlgorithmScore(1L))
  }

  test("AlgorithmScore * scalar should multiply properly") {
    assertEquals(AlgorithmScore.One * 5, AlgorithmScore(5L))
  }

  test("AlgorithmScore + AlgorithmScore should sum properly") {
    assertEquals(AlgorithmScore.One + AlgorithmScore(4L), AlgorithmScore(5L))
  }

  // ----- AlgorithmStep

  test("AlgorithmStep.eq should work correctly") {
    assert(AlgorithmStep(4, 4, 5) === AlgorithmStep(4, 4, 5))
  }

  test("AlgorithmStep.show should work correctly") {
    val score = AlgorithmStep(4, 4, 5)
    assertEquals(score.show, "AlgorithmStep(i = 4, j = 4, k = 5)")
  }

  test("AlgorithmStep should have Up operation") {
    val step = AlgorithmStep(5, 5)
    assertEquals(step.up, AlgorithmStep(4, 5, 1))
  }

  test("AlgorithmStep should have UpLeft operation") {
    val step = AlgorithmStep(5, 5)
    assertEquals(step.upLeft, AlgorithmStep(4, 4, 1))
  }

  test("AlgorithmStep should have Left operation") {
    val step = AlgorithmStep(5, 5)
    assertEquals(step.left, AlgorithmStep(5, 4, 1))
  }
}
