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

import org.typelevel.log4cats.slf4j._

import java.nio.file._
import java.{ time => jt }

object App extends IOApp.Simple {

  final type Filter           = Quotation => Boolean
  final type Quotations[F[_]] = F[Vector[Quotation]]

  final private val logger =
    Slf4jLogger.getLoggerFromName[IO]("prediction")

  final private val console =
    Slf4jLogger.getLoggerFromName[IO]("console")

  override def run: IO[Unit] =
    load[IO]("COTAHIST_BOVA11.txt").memoize.flatMap { quotations =>
      // format: off
      testOn(2020, quotations) *>
      testOn(2019, quotations) *>
      testOn(2018, quotations) *>
      testOn(2017, quotations) *>
      testOn(2016, quotations) *>
      testOn(2015, quotations) *>
      testOn(2014, quotations) *>
      testOn(2013, quotations) *>
      testOn(2012, quotations) *>
      testOn(2011, quotations)
      // format: on
    }

  def testOn(year: Int, quotations: Quotations[IO]): IO[Unit] =
    // format: off
        monthlyYear2(quotations, year, year - 1) *>
       biweeklyYear2(quotations, year, year - 1) *>
         weeklyYear2(quotations, year, year - 1) *>
    goldenNumberYear(quotations, year, year - 1) *>
       biweeklyYear3(quotations, year, year - 1) *>
    logger.info("") *> console.info("")
    // format: on

  // -----

  def monthlyYear(quotations: Quotations[IO], actual: Int, reference: Int): IO[Unit] =
    for {
      _       <- console.info(show"Predicting with 1 month of $actual in $reference")
      guesses <- (1 to 12).toList
                   .parTraverse { month =>
                     application[IO](quotations, monthFilter(month, actual), yearFilter(reference))
                   }
                   .nested
                   .filter(identity)
                   .value
                   .map(_.size)
      _       <- console.info(show"Guessed $guesses correctly out of 12")
    } yield ()

  def monthlyYear2(quotations: Quotations[IO], actual: Int, reference: Int): IO[Unit] =
    for {
      _       <- console.info(show"Predicting with 1 month of $actual in $reference")
      guesses <- (1 to 12).toList
                   .parTraverse { month =>
                     application[IO](quotations, monthFilter(month, actual), upToYearFilter(reference))
                   }
                   .nested
                   .filter(identity)
                   .value
                   .map(_.size)
      _       <- logger.info(show"2010 até $actual\tMeses de $reference\t12\t$guesses")
      _       <- console.info(show"Guessed $guesses correctly out of 12")
    } yield ()

  def biweeklyYear(quotations: Quotations[IO], actual: Int, reference: Int): IO[Unit] =
    for {
      _       <- console.info(show"Predicting with 14 days of $actual in $reference")
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
      _       <- console.info(show"Guessed $guesses correctly out of 12")
    } yield ()

  def biweeklyYear2(quotations: Quotations[IO], actual: Int, reference: Int): IO[Unit] =
    for {
      _                <- console.info(show"Predicting with 10 days of $actual in $reference")
      result           <- quotations.flatMap { quotations =>
                            quotations
                              .filter(yearFilter(actual))
                              .sliding(10, 10)
                              .filter(_.size === 10)
                              .toList
                              .parTraverse { actual =>
                                application2[IO](IO(quotations), IO(actual), upToYearFilter(reference))
                              }
                          }
      (guesses, total) <- IO(result.filter(identity).size -> result.size)
      _                <- logger.info(show"2010 até $actual\tJanela 10 dias de $reference\t$total\t$guesses")
      _                <- console.info(show"Guessed $guesses correctly out of $total")
    } yield ()

  def biweeklyYear3(quotations: Quotations[IO], actual: Int, reference: Int): IO[Unit] =
    for {
      _                <- console.info(show"Predicting with 20 days of $actual in $reference")
      result           <- quotations.flatMap { quotations =>
                            quotations
                              .filter(yearFilter(actual))
                              .sliding(20, 20)
                              .filter(_.size === 20)
                              .toList
                              .parTraverse { actual =>
                                application2[IO](IO(quotations), IO(actual), upToYearFilter(reference))
                              }
                          }
      (guesses, total) <- IO(result.filter(identity).size -> result.size)
      _                <- logger.info(show"2010 até $actual\tJanela 20 dias de $reference\t$total\t$guesses")
      _                <- console.info(show"Guessed $guesses correctly out of $total")
    } yield ()

  def weeklyYear(quotations: Quotations[IO], actual: Int, reference: Int): IO[Unit] =
    for {
      _       <- console.info(show"Predicting with 7 days of $actual in $reference")
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
      _       <- console.info(show"Guessed $guesses correctly out of 48")
    } yield ()

  def weeklyYear2(quotations: Quotations[IO], actual: Int, reference: Int): IO[Unit] =
    for {
      _                <- console.info(show"Predicting with 5 days of $actual in $reference")
      result           <- quotations.flatMap { quotations =>
                            quotations
                              .filter(yearFilter(actual))
                              .sliding(5, 5)
                              .filter(_.size === 5)
                              .toList
                              .parTraverse { actual =>
                                application2[IO](IO(quotations), IO(actual), upToYearFilter(reference))
                              }
                          }
      (guesses, total) <- IO(result.filter(identity).size -> result.size)
      _                <- logger.info(show"2010 até $actual\tJanela 5 dias de $reference\t$total\t$guesses")
      _                <- console.info(show"Guessed $guesses correctly out of $total")
    } yield ()

  def goldenNumberYear(quotations: Quotations[IO], actual: Int, reference: Int): IO[Unit] =
    for {
      _                <- console.info(show"Predicting with 17 days of $actual in $reference")
      result           <- quotations.flatMap { quotations =>
                            quotations
                              .filter(yearFilter(actual))
                              .sliding(17, 17)
                              .filter(_.size === 17)
                              .toList
                              .parTraverse { actual =>
                                application2[IO](IO(quotations), IO(actual), upToYearFilter(reference))
                              }
                          }
      (guesses, total) <- IO(result.filter(identity).size -> result.size)
      _                <- logger.info(show"2010 até $actual\tJanela 17 dias de $reference\t$total\t$guesses")
      _                <- console.info(show"Guessed $guesses correctly out of $total")
    } yield ()

  // ----- Filters

  def yearFilter(year: Int)(quotation: Quotation): Boolean = {
    val date = quotation.exchangeDate.value
    date.getYear === year
  }

  def upToYearFilter(year: Int)(quotation: Quotation): Boolean = {
    val date = quotation.exchangeDate.value
    date.getYear <= year
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

  def application2[F[_]: Async](quotations: Quotations[F], actual: Quotations[F], referenceFilter: Filter): F[Boolean] =
    for {
      predictor        <- quotationPredictor
      actual           <- actual
      reference        <- prepare(quotations, referenceFilter)
      lastExchangeDate <- getLastExchangeDate(actual)
      expected         <- getExpectedResult(lastExchangeDate)(quotations)
      predicted        <- predictor.predict(actual, reference)
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
