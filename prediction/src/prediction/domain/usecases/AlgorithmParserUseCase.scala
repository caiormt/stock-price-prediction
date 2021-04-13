package prediction.domain.usecases

trait AlgorithmParserUseCase[F[_], Input, Token] {
  def parse(input: Input): F[Token]
}
