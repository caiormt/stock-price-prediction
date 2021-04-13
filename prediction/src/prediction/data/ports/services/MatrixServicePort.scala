package prediction.data.ports.services

trait MatrixServicePort[F[_], Matrix[_], Score] {

  final type Coordinates  = (Int, Int)
  final type Cell         = (Coordinates, Score)
  final type MatrixFilter = (Cell) => Boolean

  /**
    * Creates an empty matrix N x M.
    *
    * @param n Number of rows
    * @param m Number of columns
    * @return Empty matrix allocated
    */
  def empty(n: Int, m: Int): F[Matrix[Score]]

  /**
    * Places the value in the position (i, j).
    *
    * @param matrix Matrix where data is set to
    * @param i Row index
    * @param j Column index
    * @param value Value
    */
  def set(matrix: Matrix[Score], i: Int, j: Int, value: Score): F[Unit]

  /**
    * Retrieve the value in the position (i, j).
    *
    * @param matrix Matrix where data is retrieved from
    * @param i Row index
    * @param j Column index
    * @return Score
    */
  def get(matrix: Matrix[Score], i: Int, j: Int): F[Score]

  /**
    * Returns the size of the matrix.
    *
    * @param matrix Matrix where size is retrieved from
    * @return The size of the matrix
    */
  def size(matrix: Matrix[Score]): F[(Int, Int)]

  /**
    * Returns the coordinates where the score is maximum.
    *
    * @param matrix Matrix where coordinates are retrieved from
    * @param f Function to filter choosen values
    * @return The coordinates from maximum value
    */
  def maximumBy(matrix: Matrix[Score], f: MatrixFilter): F[(Int, Int)]
}
