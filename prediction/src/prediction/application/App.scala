package prediction.application

import cats._
import cats.implicits._

import cats.effect._
import cats.effect.implicits._

import fs2.Stream
import fs2.text

import io.chrisdavenport.cats.time._

import org.typelevel.log4cats._
import org.typelevel.log4cats.slf4j._

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

import natchez.TraceValue._
import natchez._
import natchez.log._

import java.nio.file._
import java.{ time => jt }

object App extends IOApp.Simple {

  final type Filter                = Quotation => Boolean
  final type Quotations[F[_]]      = F[Vector[Quotation]]
  final type AlgorithmParser[F[_]] = AlgorithmParserUseCase[F, Quotation, AlgorithmToken]

  override def run: IO[Unit] =
    Slf4jLogger.fromName[IO]("trace").flatMap { implicit logger =>
      Log.entryPoint[IO]("prediction").root("run").use { root =>
        Trace.ioTrace(root).flatMap { implicit trace =>
          application[IO]
        }
      }
    }

  def application[F[_]: Async: Trace]: F[Unit] = {
    def run(quotations: Quotations[F])(implicit logger: Logger[F], algorithmParser: AlgorithmParser[F]) =
      // format: off
      testCase[F](2020, quotations) *>
      testCase[F](2019, quotations) *>
      testCase[F](2018, quotations) *>
      testCase[F](2017, quotations) *>
      testCase[F](2016, quotations) *>
      testCase[F](2015, quotations) *>
      testCase[F](2014, quotations) *>
      testCase[F](2013, quotations) *>
      testCase[F](2012, quotations) *>
      testCase[F](2011, quotations)
      // format: on

    def go(variance: BigDecimal, quotations: Quotations[F])(implicit logger: Logger[F]) =
      // format: off
      logger.info(show"Alfabeto 3  - Variação $variance") *>
        Sync[F].delay(new AlgorithmParserService3[F]).flatMap(implicit algorithmParser => run(quotations)) *>
      logger.info(show"\n\nAlfabeto 5  - Variação $variance") *>
        Sync[F].delay(new AlgorithmParserService5[F](variance)).flatMap(implicit algorithmParser => run(quotations)) *>
      logger.info(show"\n\nAlfabeto 7  - Variação $variance") *>
        Sync[F].delay(new AlgorithmParserService7[F](variance)).flatMap(implicit algorithmParser => run(quotations)) *>
      logger.info(show"\n\nAlfabeto 9  - Variação $variance") *>
        Sync[F].delay(new AlgorithmParserService9[F](variance)).flatMap(implicit algorithmParser => run(quotations)) *>
      logger.info(show"\n\nAlfabeto 11 - Variação $variance") *>
        Sync[F].delay(new AlgorithmParserService11[F](variance)).flatMap(implicit algorithmParser => run(quotations))
      // format: on

    load[F]("COTAHIST_BOVA11.txt").memoize.flatMap { quotations =>
      Slf4jLogger.fromName[F]("report").flatMap { implicit logger =>
        // format: off
        go(0.2, quotations) *>
        go(0.5, quotations) *>
        go(1.0, quotations) *>
        go(5.0, quotations) *>
        go(10.0, quotations)
        // format: on
      }
    }
  }

  def testCase[F[_]: Async: Logger: Trace: AlgorithmParser](year: Int, quotations: Quotations[F]): F[Unit] =
    // format: off
         monthlyYear(quotations, year, year - 1) *>
        biweeklyYear(quotations, year, year - 1) *>
          weeklyYear(quotations, year, year - 1) *>
    goldenNumberYear(quotations, year, year - 1)
    // format: on

  // -----

  def monthlyYear[F[_]: Async: Logger: Trace: AlgorithmParser](
      quotations: Quotations[F],
      actual: Int,
      reference: Int): F[Unit] =
    Trace[F].span("monthly") {
      for {
        _                <- Trace[F].put(("actual", actual), ("reference", reference))
        result           <- quotations.flatMap { quotations =>
                              (1 to 12).toList
                                .parTraverse { month =>
                                  val actualQuotations = quotations.filter(monthFilter(month, actual))
                                  application[F](quotations.pure[F], actualQuotations.pure[F], upToYearFilter(reference))
                                }
                            }
        (guesses, total) <- Applicative[F].pure(result.count(identity) -> result.size)
        _                <- Trace[F].put(("total", total), ("guesses", guesses))
        _                <- Logger[F].info(show"2010 até $actual\tMeses de $reference\t$total\t$guesses")
      } yield ()
    }

  def biweeklyYear[F[_]: Async: Logger: Trace: AlgorithmParser](
      quotations: Quotations[F],
      actual: Int,
      reference: Int
  ): F[Unit] =
    Trace[F].span("biweekly") {
      for {
        _                <- Trace[F].put(("actual", actual), ("reference", reference))
        result           <- quotations.flatMap { quotations =>
                              quotations
                                .filter(yearFilter(actual))
                                .sliding(10, 10)
                                .filter(_.size === 10)
                                .toList
                                .parTraverse { actual =>
                                  application[F](quotations.pure[F], actual.pure[F], upToYearFilter(reference))
                                }
                            }
        (guesses, total) <- Applicative[F].pure(result.count(identity) -> result.size)
        _                <- Trace[F].put(("total", total), ("guesses", guesses))
        _                <- Logger[F].info(show"2010 até $actual\tJanela 10 dias de $reference\t$total\t$guesses")
      } yield ()
    }

  def weeklyYear[F[_]: Async: Logger: Trace: AlgorithmParser](
      quotations: Quotations[F],
      actual: Int,
      reference: Int
  ): F[Unit] =
    Trace[F].span("weekly") {
      for {
        _                <- Trace[F].put(("actual", actual), ("reference", reference))
        result           <- quotations.flatMap { quotations =>
                              quotations
                                .filter(yearFilter(actual))
                                .sliding(5, 5)
                                .filter(_.size === 5)
                                .toList
                                .parTraverse { actual =>
                                  application[F](quotations.pure[F], actual.pure[F], upToYearFilter(reference))
                                }
                            }
        (guesses, total) <- Applicative[F].pure(result.count(identity) -> result.size)
        _                <- Trace[F].put(("total", total), ("guesses", guesses))
        _                <- Logger[F].info(show"2010 até $actual\tJanela 5 dias de $reference\t$total\t$guesses")
      } yield ()
    }

  def goldenNumberYear[F[_]: Async: Logger: Trace: AlgorithmParser](
      quotations: Quotations[F],
      actual: Int,
      reference: Int
  ): F[Unit] =
    Trace[F].span("golden-number") {
      for {
        _                <- Trace[F].put(("actual", actual), ("reference", reference))
        result           <- quotations.flatMap { quotations =>
                              quotations
                                .filter(yearFilter(actual))
                                .sliding(17, 17)
                                .filter(_.size === 17)
                                .toList
                                .parTraverse { actual =>
                                  application[F](quotations.pure[F], actual.pure[F], upToYearFilter(reference))
                                }
                            }
        (guesses, total) <- Applicative[F].pure(result.count(identity) -> result.size)
        _                <- Trace[F].put(("total", total), ("guesses", guesses))
        _                <- Logger[F].info(show"2010 até $actual\tJanela 17 dias de $reference\t$total\t$guesses")
      } yield ()
    }

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

  def application[F[_]: Async: Trace: AlgorithmParser](
      quotations: Quotations[F],
      actual: Quotations[F],
      referenceFilter: Filter
  ): F[Boolean] =
    for {
      predictor        <- quotationPredictor
      actual           <- actual
      reference        <- prepare(quotations, referenceFilter)
      lastExchangeDate <- getLastExchangeDate(actual)
      expected         <- getExpectedResult(implicitly[AlgorithmParser[F]])(lastExchangeDate)(quotations)
      predicted        <- predictor.predict(actual, reference)
    } yield expected === predicted

  def quotationPredictor[F[_]: Async: Trace: AlgorithmParser]: F[QuotationPredictorUseCase[F]] =
    for {
      matrix           <- Sync[F].delay(new BreezeMatrixAdapter[F, AlgorithmScore])
      scoreCalculator  <- Sync[F].delay(new ScoreCalculatorService[F])
      alignmentFinder  <- Sync[F].delay(new AlignmentFinderService(matrix))
      optimalAlignment <- Sync[F].delay(new OptimalAlignmentBuilderService(matrix, scoreCalculator))
      usecase          <- Sync[F].delay {
                            new QuotationPredictorService(implicitly[AlgorithmParser[F]], optimalAlignment, alignmentFinder)
                          }
    } yield usecase

  def quotationAligner[F[_]: Async: Trace: AlgorithmParser]: F[QuotationAlignerUseCase[F]] =
    for {
      matrix           <- Sync[F].delay(new BreezeMatrixAdapter[F, AlgorithmScore])
      vector           <- Sync[F].delay(new BreezeVectorAdapter[F, AlgorithmToken])
      score            <- Sync[F].delay(new ScoreCalculatorService[F])
      alignmentFinder  <- Sync[F].delay(new AlignmentFinderService(matrix))
      optimalAlignment <- Sync[F].delay(new OptimalAlignmentBuilderService(matrix, score))
      optimalAlignment <- Sync[F].delay {
                            new OptimalAlignmentAlignerService(matrix, vector, score, alignmentFinder, optimalAlignment)
                          }
      usecase          <- Sync[F].delay(new QuotationAlignerService(implicitly[AlgorithmParser[F]], optimalAlignment))
    } yield usecase

  def load[F[_]: Async: Trace](filename: String): F[Vector[Quotation]] =
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

  def getExpectedResult[F[_]: Async](
      algorithmParser: AlgorithmParserUseCase[F, Quotation, AlgorithmToken]
  )(date: jt.LocalDateTime)(quotations: F[Vector[Quotation]]): F[Option[Alphabet]] = {
    def before(date: jt.LocalDateTime)(quotation: Quotation): Boolean =
      Order[jt.LocalDateTime].lteqv(quotation.exchangeDate.value, date)

    Stream
      .evalSeq(quotations)
      .dropWhile(before(date))
      .take(1)
      .evalMap(algorithmParser.parse)
      .map(_.value)
      .compile
      .last
  }
}
