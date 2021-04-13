package prediction.adapter.breeze

import cats.implicits._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._

import breeze.linalg._
import breeze.math._

import scala.reflect._

// $COVERAGE-OFF$
object algorithm {
  implicit val classTagForAlgorithmScore: ClassTag[AlgorithmScore] =
    implicitly[ClassTag[Long]].asInstanceOf[ClassTag[AlgorithmScore]]

  implicit val classTagForAlgorithmToken: ClassTag[AlgorithmToken] =
    implicitly[ClassTag[Alphabet]].asInstanceOf[ClassTag[AlgorithmToken]]

  implicit val ringForAlgorithmScore: Ring[AlgorithmScore] = new Ring[AlgorithmScore] {
    override def zero: AlgorithmScore =
      AlgorithmScore.Empty

    override def one: AlgorithmScore =
      AlgorithmScore.One

    override def ==(a: AlgorithmScore, b: AlgorithmScore): Boolean =
      a === b

    override def !=(a: AlgorithmScore, b: AlgorithmScore): Boolean =
      a =!= b

    override def +(a: AlgorithmScore, b: AlgorithmScore): AlgorithmScore =
      a + b

    override def -(a: AlgorithmScore, b: AlgorithmScore): AlgorithmScore =
      AlgorithmScore(a.value - b.value)

    override def *(a: AlgorithmScore, b: AlgorithmScore): AlgorithmScore =
      a * b

    override def %(a: AlgorithmScore, b: AlgorithmScore): AlgorithmScore =
      AlgorithmScore(a.value % b.value)

    implicit override val normImpl: norm.Impl[AlgorithmScore, Double]    = new norm.Impl[AlgorithmScore, Double] {
      def apply(v1: AlgorithmScore): Double = v1.value.abs.toDouble
    }
  }

  implicit val semiringForAlgorithmToken: Semiring[AlgorithmToken] = new Semiring[AlgorithmToken] {
    override def zero: AlgorithmToken =
      AlgorithmToken.Empty

    override def one: AlgorithmToken =
      throw new UnsupportedOperationException("Operation not defined to AlgorithmToken")

    override def ==(a: AlgorithmToken, b: AlgorithmToken): Boolean =
      a === b

    override def !=(a: AlgorithmToken, b: AlgorithmToken): Boolean =
      a =!= b

    override def +(a: AlgorithmToken, b: AlgorithmToken): AlgorithmToken =
      throw new UnsupportedOperationException("Operation not defined to AlgorithmToken")

    override def *(a: AlgorithmToken, b: AlgorithmToken): AlgorithmToken =
      throw new UnsupportedOperationException("Operation not defined to AlgorithmToken")
  }
}
// $COVERAGE-ON$
