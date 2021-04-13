package prediction.domain.usecases

trait OptimalAlignmentAlignerUseCase[F[_], Sequence, Alignment] {
  def align(left: Sequence, right: Sequence): F[Alignment]
}
