package prediction.data.ports.services

trait VectorServicePort[F[_], Vec[_], Token] {

  /**
    * Creates an empty vector N.
    *
    * @param n Number of cells
    * @return Empty vector allocated.
    */
  def empty(n: Int): F[Vec[Token]]

  /**
    * Places the value in the position `i`.
    *
    * @param vector Vector where data is set to
    * @param i Index
    * @param value Token
    */
  def set(vector: Vec[Token], i: Int, value: Token): F[Unit]

  /**
    * Retrieve the value in the position `i`.
    *
    * @param vector Vector where data is retrieved from
    * @param i Index
    * @return Token
    */
  def get(vector: Vec[Token], i: Int): F[Token]

  /**
    * Transforms the current collection into Scala's Vector.
    *
    * @param vector Vector where data is transfered to scala's vector.
    * @return Scala's Vector
    */
  def toScalaVector(vector: Vec[Token]): F[Vector[Token]]
}
