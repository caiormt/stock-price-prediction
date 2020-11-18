package prediction.ext

import breeze.math._

object Semiring {
  @inline def apply[A](implicit ev: Semiring[A]): Semiring[A] = ev

  implicit def semiringOps[A: Semiring](a: A): SemiringOps[A] =
    new SemiringOps[A](a)

  final class SemiringOps[A: Semiring](private val a: A) extends Serializable {
    def +(b: A): A = Semiring[A].+(a, b)
  }
}
