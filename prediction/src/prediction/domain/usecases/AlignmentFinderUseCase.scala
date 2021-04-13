package prediction.domain.usecases

trait AlignmentFinderUseCase[F[_], Matrix[_], Score] {
  def findCoordinates(matrix: Matrix[Score]): F[(Int, Int)]
}
