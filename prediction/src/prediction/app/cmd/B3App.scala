package prediction.app.cmd

import cats._
import cats.implicits._

import cats.effect.Console.implicits._
import cats.effect._

import prediction.data.ports.repositories._
import prediction.data.usecases._
import prediction.data.usecases.b3._
import prediction.infra.repositories._
import prediction.infra.services._
import prediction.presentation.controllers._

import java.nio.file.Paths

object B3App extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    app[IO].as(ExitCode.Success)

  def app[F[_]: Sync: Parallel: ContextShift: Console]: F[Unit] =
    loop[F]

  def setup[F[_]: Sync: Parallel]: F[ComandLineController[F]] =
    for {
      optimalAlignment <- semiGlobalOptimalAlignment[F]
      parser           <- B3AlgorithmParser.make[F]
      predictor        <- DefaultQuotationPredictor.make(parser, optimalAlignment)
      controller       <- ComandLineController.make(predictor)
    } yield controller

  def semiGlobalOptimalAlignment[F[_]: Sync: Parallel]: F[BreezeOptimalAlignment[F]] =
    for {
      matrix    <- BreezeMatrixService.make[F]
      vector    <- BreezeVectorService.make[F]
      score     <- B3ScoreCalculator.make[F]
      alignment <- SemiGlobalOptimalAlignment.make(matrix, vector, score)
    } yield alignment

  def loop[F[_]: Sync: Parallel: ContextShift: Console]: F[Unit] =
    Blocker[F].use { blocker =>
      def ask(str: String): F[String] =
        Console[F].putStrLn(str) *> Console[F].readLn

      def line: F[Unit] =
        Console[F].putStrLn("")

      def menu: F[Unit] = {
        val text = """|--------------------------------------
                      || Stock Price Prediction - B3 Layout |
                      |--------------------------------------
                      |
                      |0 - Exit
                      |1 - Predict status of next quotation
                      |2 - Align quotation history
                      |
                      |Choose an option: """.stripMargin

        ask(text).flatMap {
          case "0" =>
            Console[F].putStrLn("See ya!") *> Applicative[F].unit
          case "1" =>
            predict *> line >> menu
          case "2" =>
            align *> line >> menu
          case _   =>
            Console[F].putError("Unknown option!") >> menu
        }
      }

      def quotationRepositories: F[(QuotationRepository[F], QuotationRepository[F])] =
        for {
          firstSequencePath         <- ask("First sequence path: ").map(Paths.get(_))
          secondSequencePath        <- ask("Second sequence path: ").map(Paths.get(_))
          firstQuotationRepository  <- B3FileQuotationRepository.make[F](firstSequencePath, blocker)
          secondQuotationRepository <- B3FileQuotationRepository.make[F](secondSequencePath, blocker)
        } yield firstQuotationRepository -> secondQuotationRepository

      def predict: F[Unit]                                                           =
        for {
          controller      <- setup[F]
          (first, second) <- quotationRepositories
          predicted       <- controller.predict(first, second)
          _               <- Console[F].putStrLn(predicted)
        } yield ()

      def align: F[Unit] =
        for {
          controller      <- setup[F]
          (first, second) <- quotationRepositories
          align           <- controller.align(first, second)
          _               <- Console[F].putStrLn(align.first)
          _               <- Console[F].putStrLn(align.second)
        } yield ()

      menu
    }
}
