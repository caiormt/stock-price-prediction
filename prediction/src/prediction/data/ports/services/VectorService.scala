package prediction.data.ports.services

import prediction.domain.entities.algorithm._

/**
  * Represents the ability to manipulate Vector in form of C inside effect F.
  */
trait VectorService[F[_], C[_]] {

  /**
    * Creates an empty vector N.
    *
    * @param n Number of cells
    * @return Empty vector allocated.
    */
  def empty(n: Int): F[C[AlgorithmToken]]

  /**
    * Places the value in the position n.
    *
    * @param vector Vector where data is set to
    * @param n Cell index
    * @param value Score
    */
  def set(vector: C[AlgorithmToken], n: SequenceIndex, value: AlgorithmToken): F[Unit]

  /**
    * Transforms the current collection into Scala's Vector.
    *
    * @param vector Vector where data is transfered to scala's vector.
    * @return Scala's Vector
    */
  def toVector(vector: C[AlgorithmToken]): F[Vector[AlgorithmToken]]
}
