package prediction.domain.usecases

trait OptimalAlignmentBuilderUseCase[F[_], Sequence, Matrix[_], Score] {
  def build(left: Sequence, right: Sequence): F[Matrix[Score]]
}
