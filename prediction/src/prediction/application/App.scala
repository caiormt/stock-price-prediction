package prediction.application

import cats._
import cats.implicits._

import cats.effect._
import cats.effect.implicits._

import fs2.Stream
import fs2.text

import io.chrisdavenport.cats.time._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._
import prediction.domain.entities.quotation._
import prediction.domain.usecases._

import prediction.data.services._

import prediction.adapter.atto.pipes._
import prediction.adapter.breeze.algorithm._
import prediction.adapter.repositories._
import prediction.adapter.repositories.models.b3._
import prediction.adapter.repositories.parsers.b3._
import prediction.adapter.services._

import java.nio.file._
import java.{ time => jt }

object App extends IOApp.Simple {

  final type Filter           = Quotation => Boolean
  final type Quotations[F[_]] = F[Vector[Quotation]]

  override def run: IO[Unit] =
    load[IO]("COTAHIST_BOVA11.txt").memoize.flatMap { quotations =>
      // format: off
      runCase(   month2020Year2019(quotations)) *>
      runCase(biweekly2020Year2019(quotations)) *>
      runCase(  weekly2020Year2019(quotations)) *>
      runCase(   month2019Year2018(quotations)) *>
      runCase(biweekly2019Year2018(quotations)) *>
      runCase(  weekly2019Year2018(quotations)) *>
      runCase(   month2018Year2017(quotations)) *>
      runCase(biweekly2018Year2017(quotations)) *>
      runCase(  weekly2018Year2017(quotations)) *>
      runCase(   month2017Year2016(quotations)) *>
      runCase(biweekly2017Year2016(quotations)) *>
      // runCase(  weekly2017Year2016(quotations)) *>
      runCase(   month2016Year2015(quotations)) *>
      runCase(biweekly2016Year2015(quotations)) *>
      runCase(  weekly2016Year2015(quotations))
      // format: on
    }

  def month2020Year2019(quotations: Quotations[IO]): IO[Unit] =
    monthlyYear(quotations, 2020, 2019)

  def biweekly2020Year2019(quotations: Quotations[IO]): IO[Unit] =
    biweeklyYear(quotations, 2020, 2019)

  def weekly2020Year2019(quotations: Quotations[IO]): IO[Unit] =
    weeklyYear(quotations, 2020, 2019)

  def month2019Year2018(quotations: Quotations[IO]): IO[Unit] =
    monthlyYear(quotations, 2019, 2018)

  def biweekly2019Year2018(quotations: Quotations[IO]): IO[Unit] =
    biweeklyYear(quotations, 2019, 2018)

  def weekly2019Year2018(quotations: Quotations[IO]): IO[Unit] =
    weeklyYear(quotations, 2019, 2018)

  def month2018Year2017(quotations: Quotations[IO]): IO[Unit] =
    monthlyYear(quotations, 2018, 2017)

  def biweekly2018Year2017(quotations: Quotations[IO]): IO[Unit] =
    biweeklyYear(quotations, 2018, 2017)

  def weekly2018Year2017(quotations: Quotations[IO]): IO[Unit] =
    weeklyYear(quotations, 2018, 2017)

  def month2017Year2016(quotations: Quotations[IO]): IO[Unit] =
    monthlyYear(quotations, 2017, 2016)

  def biweekly2017Year2016(quotations: Quotations[IO]): IO[Unit] =
    biweeklyYear(quotations, 2017, 2016)

  def weekly2017Year2016(quotations: Quotations[IO]): IO[Unit] =
    weeklyYear(quotations, 2017, 2016)

  def month2016Year2015(quotations: Quotations[IO]): IO[Unit] =
    monthlyYear(quotations, 2016, 2015)

  def biweekly2016Year2015(quotations: Quotations[IO]): IO[Unit] =
    biweeklyYear(quotations, 2016, 2015)

  def weekly2016Year2015(quotations: Quotations[IO]): IO[Unit] =
    weeklyYear(quotations, 2016, 2015)

  // -----

  def monthlyYear(quotations: Quotations[IO], actual: Int, reference: Int): IO[Unit] =
    for {
      _       <- IO.println(show"Predicting with 1 month of $actual in $reference")
      guesses <- (1 to 12).toList
                   .parTraverse { month =>
                     application[IO](quotations, monthFilter(month, actual), yearFilter(reference))
                   }
                   .nested
                   .filter(identity)
                   .value
                   .map(_.size)
      _       <- IO.println(show"Guessed $guesses correctly out of 12")
    } yield ()

  def biweeklyYear(quotations: Quotations[IO], actual: Int, reference: Int): IO[Unit] =
    for {
      _       <- IO.println(show"Predicting with 14 days of $actual in $reference")
      guesses <- (1 to 12).toList
                   .parFlatTraverse { month =>
                     (0 to 1).toList.parTraverse { week =>
                       application[IO](quotations, biweeklyFilter(week, month, actual), yearFilter(reference))
                     }
                   }
                   .nested
                   .filter(identity)
                   .value
                   .map(_.size)
      _       <- IO.println(show"Guessed $guesses correctly out of 24")
    } yield ()

  def weeklyYear(quotations: Quotations[IO], actual: Int, reference: Int): IO[Unit] =
    for {
      _       <- IO.println(show"Predicting with 7 days of $actual in $reference")
      guesses <- (1 to 12).toList
                   .parFlatTraverse { month =>
                     (0 to 3).toList.parTraverse { week =>
                       application[IO](quotations, weeklyFilter(week, month, actual), yearFilter(reference))
                     }
                   }
                   .nested
                   .filter(identity)
                   .value
                   .map(_.size)
      _       <- IO.println(show"Guessed $guesses correctly out of 48")
    } yield ()

  def runCase(f: IO[Unit]): IO[Unit] =
    start *> f <* end

  def start: IO[Unit] =
    IO.println("")

  def end: IO[Unit] =
    IO.println("--------------------------------------------------")

  // ----- Filters

  def yearFilter(year: Int)(quotation: Quotation): Boolean = {
    val date = quotation.exchangeDate.value
    date.getYear === year
  }

  def monthFilter(month: Int, year: Int)(quotation: Quotation): Boolean = {
    val date = quotation.exchangeDate.value
    yearFilter(year)(quotation) && date.getMonthValue === month
  }

  def biweeklyFilter(week: Int, month: Int, year: Int)(quotation: Quotation): Boolean = {
    val date       = quotation.exchangeDate.value
    val dayOfMonth = date.getDayOfMonth
    monthFilter(month, year)(quotation) && dayOfMonth > (16 * week) && dayOfMonth <= (16 * (week + 1))
  }

  def weeklyFilter(week: Int, month: Int, year: Int)(quotation: Quotation): Boolean = {
    val date       = quotation.exchangeDate.value
    val dayOfMonth = date.getDayOfMonth
    monthFilter(month, year)(quotation) && dayOfMonth > (8 * week) && dayOfMonth <= (8 * (week + 1))
  }

  // ----- Base application

  def application[F[_]: Async](quotations: Quotations[F], actualFilter: Filter, referenceFilter: Filter): F[Boolean] =
    for {
      predictor           <- quotationPredictor
      (actual, reference) <- (prepare(quotations, actualFilter), prepare(quotations, referenceFilter)).parTupled
      lastExchangeDate    <- getLastExchangeDate(actual)
      expected            <- getExpectedResult(lastExchangeDate)(quotations)
      predicted           <- predictor.predict(actual, reference)
      // aligner             <- quotationAligner
      // alignment           <- aligner.align(actual, reference)
      // _                   <- Sync[F].blocking(println(show"Expected:  $expected\nPredicted: $predicted"))
      // _                   <- Sync[F].blocking(println(alignment.show))
    } yield expected === predicted

  def quotationPredictor[F[_]: Async]: F[QuotationPredictorUseCase[F]] =
    for {
      matrix           <- Sync[F].delay(new BreezeMatrixAdapter[F, AlgorithmScore])
      scoreCalculator  <- Sync[F].delay(new ScoreCalculatorService[F])
      algorithmParser  <- Sync[F].delay(new AlgorithmParserService[F])
      alignmentFinder  <- Sync[F].delay(new AlignmentFinderService(matrix))
      optimalAlignment <- Sync[F].delay(new OptimalAlignmentBuilderService(matrix, scoreCalculator))
      usecase          <- Sync[F].delay(new QuotationPredictorService(algorithmParser, optimalAlignment, alignmentFinder))
    } yield usecase

  def quotationAligner[F[_]: Async]: F[QuotationAlignerUseCase[F]] =
    for {
      matrix           <- Sync[F].delay(new BreezeMatrixAdapter[F, AlgorithmScore])
      vector           <- Sync[F].delay(new BreezeVectorAdapter[F, AlgorithmToken])
      score            <- Sync[F].delay(new ScoreCalculatorService[F])
      algorithmParser  <- Sync[F].delay(new AlgorithmParserService[F])
      alignmentFinder  <- Sync[F].delay(new AlignmentFinderService(matrix))
      optimalAlignment <- Sync[F].delay(new OptimalAlignmentBuilderService(matrix, score))
      optimalAlignment <- Sync[F].delay {
                            new OptimalAlignmentAlignerService(matrix, vector, score, alignmentFinder, optimalAlignment)
                          }
      usecase          <- Sync[F].delay(new QuotationAlignerService(algorithmParser, optimalAlignment))
    } yield usecase

  def load[F[_]: Async](filename: String): F[Vector[Quotation]] =
    for {
      url        <- Sync[F].blocking(getClass.getClassLoader.getResource(filename))
      path       <- Sync[F].blocking(Paths.get(url.toURI))
      repository <- Sync[F].delay(new FileQuotationAdapter[F](path, pipeline))
      quotations <- repository.load()
    } yield quotations.sortBy(_.exchangeDate)

  def pipeline[F[_]]: fs2.Pipe[F, Byte, Quotation] = stream =>
    stream
      .through(text.utf8Decode)
      .through(text.lines)
      .through(parseN(entry))
      .collect(collector)

  def prepare[F[_]: Apply](quotations: F[Vector[Quotation]], f: Filter): F[Vector[Quotation]] =
    quotations.map(_.filter(f))

  def getLastExchangeDate[F[_]: Sync](quotations: Vector[Quotation]): F[jt.LocalDateTime] =
    Sync[F].delay(quotations.last.exchangeDate.value)

  def getExpectedResult[F[_]: Async](date: jt.LocalDateTime)(quotations: F[Vector[Quotation]]): F[Option[Alphabet]] =
    Sync[F].delay(new AlgorithmParserService[F]).flatMap { algorithmParser =>
      Stream
        .evalSeq(quotations)
        .dropWhile(q => Order[jt.LocalDateTime].lteqv(q.exchangeDate.value, date))
        .take(1)
        .evalMap(algorithmParser.parse)
        .map(_.value)
        .compile
        .last
    }
}
