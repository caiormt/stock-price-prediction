package prediction.domain.usecases

trait ScoreCalculatorUseCase[F[_], Token, Score] {
  def calculate(left: Token, right: Token): F[Score]
}
