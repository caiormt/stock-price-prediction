package prediction.app.cmd

import cats._
import cats.implicits._

import cats.effect.Console.implicits._
import cats.effect._

import prediction.data.usecases._
import prediction.domain.entities.algorithm._
import prediction.domain.usecases._
import prediction.infra.services._
import prediction.presentation.controllers._

import breeze.linalg._

object App extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    app[IO].as(ExitCode.Success)

  def app[F[_]: Sync: Parallel: Console]: F[Unit] =
    for {
      implicit0(parser: AlgorithmParser[F]) <- B3AlgorithmParser.make[F]
      alignment                             <- semiGlobalOptimalAlignment
      controller                            <- ComandLineController.make[F, DenseMatrix](alignment)
      first                                 <- ask[F]("First sequence")
      second                                <- ask[F]("Second sequence")
      alignment                             <- controller.align(first, second)
      _                                     <- present(alignment.first)
      _                                     <- present(alignment.second)
    } yield ()

  def semiGlobalOptimalAlignment[F[_]: Sync: Parallel]: F[OptimalAlignment[F, DenseMatrix]] =
    for {
      matrix    <- BreezeMatrixService.make[F]
      vector    <- BreezeVectorService.make[F]
      score     <- B3ScoreCalculator.make[F]
      alignment <- SemiGlobalOptimalAlignment.make(matrix, vector, score)
    } yield alignment

  def ask[F[_]: Sync: Console: AlgorithmParser](str: String): F[AlgorithmSequence] =
    for {
      input    <- Console[F].putStrLn(s"$str: ") *> Console[F].readLn
      sequence <- input.toVector.traverse(AlgorithmParser[F].parseChar)
    } yield AlgorithmSequence(sequence)

  def present[F[_]: Sync: Console](sequence: AlgorithmSequence): F[Unit] =
    Console[F].putStrLn(sequence.value.foldMap(_.value.value.toString))
}
