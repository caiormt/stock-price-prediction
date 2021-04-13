package prediction.data.services

import cats._
import cats.implicits._

import prediction.domain.entities.algorithm._
import prediction.domain.usecases._

import prediction.data.ports.services._

final class AlignmentFinderService[F[_]: Monad, Matrix[_]](service: MatrixServicePort[F, Matrix, AlgorithmScore])
    extends AlignmentFinderUseCase[F, Matrix, AlgorithmScore] {

  override def findCoordinates(matrix: Matrix[AlgorithmScore]): F[(Int, Int)] =
    for {
      (_, columns) <- service.size(matrix)
      coordinates  <- service.maximumBy(matrix, filter(columns - 1))
    } yield coordinates

  /**
    * Choose cells that are on the target column only.
    *
    * @param target column choosen
    * @param data tuple containing cell's coordinates and value
    * @return true if data is at expected column, false otherwise.
    */
  def filter(target: Int)(data: ((Int, Int), AlgorithmScore)): Boolean = {
    val ((_, column), _) = data
    target === column
  }
}
