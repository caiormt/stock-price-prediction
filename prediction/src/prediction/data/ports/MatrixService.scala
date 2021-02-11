package prediction.data.ports

import prediction.domain.entities.algorithm._

/**
  * Represents the ability to manipulate Matrix in form of C inside effect F.
  */
trait MatrixService[F[_], C[_]] {

  /**
    * Creates an empty matrix N x M.
    *
    * All cells are placed with AlgorithmScore.Empty value.
    *
    * @param n Number of rows
    * @param m Number of columns
    * @return Empty matrix allocated
    */
  def empty(n: Int, m: Int): F[C[AlgorithmScore]]

  /**
    * Retrieve the value in the position (i, j).
    *
    * @param matrix Matrix where data is retrieved from
    * @param i Row index
    * @param j Column index
    * @return Score
    */
  def get(matrix: C[AlgorithmScore], i: Int, j: Int): F[AlgorithmScore]

  /**
    * Places the value in the position (i, j).
    *
    * @param matrix Matrix where data is set to
    * @param i Row index
    * @param j Column index
    * @param value Score
    */
  def set(matrix: C[AlgorithmScore], i: Int, j: Int, value: AlgorithmScore): F[Unit]

  /**
    * Returns the number of rows of the matrix.
    *
    * @param matrix Matrix where number of rows are counted for
    * @return Matrix's rows size
    */
  def rows(matrix: C[AlgorithmScore]): F[Int]

  /**
    * Returns the number of columns of the matrix.
    *
    * @param matrix Matrix where number of columns are counted for
    * @return Matrix's columns size
    */
  def cols(matrix: C[AlgorithmScore]): F[Int]

  /**
    * Returns the maximum value of the Row I or Column J.
    *
    * @param matrix Matrix where data is retrieved from
    * @param i Row index
    * @param j Column index
    * @return The maximum value found
    */
  def max(matrix: C[AlgorithmScore], i: Int, j: Int): F[AlgorithmScore]

  /**
    * Returns the coordinates where the score is maximum.
    *
    * @param matrix Matrix where coordinates are retrieved from
    * @return The coordinates from maximum value
    */
  def coordinatesMaxColumn(matrix: C[AlgorithmScore], j: Int): F[(Int, Int)]
}
