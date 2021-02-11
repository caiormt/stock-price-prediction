package prediction.infra

import cats._
import cats.implicits._

import io.estatico.newtype._

import prediction.domain.entities.algorithm._

import breeze.generic._
import breeze.linalg._
import breeze.storage._

import scala.reflect._

package object services {
  implicit def coercibleClassTag[A: Coercible[B, *], B: ClassTag]: ClassTag[A] =
    implicitly[ClassTag[B]].asInstanceOf[ClassTag[A]]

  // -----

  implicit val algorithmScoreZero: Zero[AlgorithmScore] = new Zero[AlgorithmScore] {
    override def zero: AlgorithmScore = AlgorithmScore.Empty
  }

  implicit val algorithmTokenZero: Zero[AlgorithmToken] = new Zero[AlgorithmToken] {
    override def zero: AlgorithmToken = AlgorithmToken.Empty
  }

  // -----

  implicit def breezeMaxReduceForDenseVector[A: Order: ClassTag]: UFunc.UImpl[max.type, DenseVector[A], A] =
    new UFunc.UImpl[max.type, DenseVector[A], A] {
      override def apply(v: DenseVector[A]): A =
        v.toScalaVector().max
    }

  implicit def breezeMaxReduceForTransposedDenseVector[A: Order: ClassTag](implicit
      op: UFunc.UImpl[max.type, DenseVector[A], A]
  ): UFunc.UImpl[max.type, Transpose[DenseVector[A]], A] =
    new UFunc.UImpl[max.type, Transpose[DenseVector[A]], A] {
      override def apply(v: Transpose[DenseVector[A]]): A = op(v.inner)
    }
}
