package prediction

import cats.implicits._

import cats.effect._

import io.chrisdavenport.log4cats.slf4j._

import prediction.algebras._

import mainargs._

import java.nio.file.Paths
import java.{ time => jt }

object App extends IOApp {

  @main
  final case class Config(
      @arg(name = "cod-neg", short = 'c', doc = "Negotiation code")
      codNeg: String = "BOVA11",
      // ----- Longest Sequence -----
      @arg(name = "source", short = 's', doc = "Source file path for the longest sequence")
      source: String,
      @arg(name = "source-begin", short = 'b', doc = "Initial date from longest sequence in format <YYYY-MM-DD>")
      sourceBegin: Option[String] = None,
      @arg(name = "source-end", short = 'e', doc = "Last date from longest sequence in format <YYYY-MM-DD>")
      sourceEnd: Option[String] = None,
      // ----- Smallest Sequence -----
      @arg(name = "target", short = 't', doc = "Souce file path for the smallest sequence")
      target: Option[String] = None,
      @arg(name = "target-init", short = 'i', doc = "Initial date from smallest sequence in format <YYYY-MM-DD>")
      targetInit: String,
      @arg(name = "target-final", short = 'f', doc = "Last date from smallest sequence in format <YYYY-MM-DD>")
      targetFinal: String
  )

  final val formatter: jt.format.DateTimeFormatter =
    jt.format.DateTimeFormatter.ISO_DATE

  override def run(args: List[String]): IO[ExitCode] =
    IO(ParserForClass[Config].constructOrExit(args, allowPositional = true, docsOnNewLine = true))
      .flatMap(program[IO])
      .as(ExitCode.Success)

  private def program[F[_]: Concurrent: ContextShift](config: Config): F[Unit] =
    Slf4jLogger.create[F].flatMap { logger =>
      for {
        _           <- logger.info("Parsing arguments")
        sourceBegin <- Sync[F].delay(config.sourceBegin.map(jt.LocalDate.parse(_, formatter)))
        sourceEnd   <- Sync[F].delay(config.sourceEnd.map(jt.LocalDate.parse(_, formatter)))
        targetInit  <- Sync[F].delay(jt.LocalDate.parse(config.targetInit, formatter))
        targetFinal <- Sync[F].delay(jt.LocalDate.parse(config.targetFinal, formatter))

        _         <- logger.info("Warming up")
        reader    <- B3Reader.make[F]
        extractor <- B3Extractor.make[F]

        _         <- logger.info("Retrieving data for longest sequence")
        originData = reader.fromFile(Paths.get(config.source)).filter(_.codNeg === config.codNeg)
        sourceData = originData
                       .filter(r => sourceBegin.map(b => r.dateOfExchange.compareTo(b) <= 0).getOrElse(true))
                       .filter(r => sourceEnd.map(e => r.dateOfExchange.compareTo(e) <= 0).getOrElse(true))
        s         <- extractor.toAlphabet(sourceData)
        _         <- logger.info(s"Long sequence:\n$s")

        _         <- logger.info("Retrieving data for smallest sequence")
        targetData = config.target
                       .fold(originData) { target =>
                         reader.fromFile(Paths.get(target)).filter(_.codNeg === config.codNeg)
                       }
                       .filter(r => r.dateOfExchange.compareTo(targetInit) >= 0)
                       .filter(r => r.dateOfExchange.compareTo(targetFinal) <= 0)
        t         <- extractor.toAlphabet(targetData)
        _         <- logger.info(s"Small sequence:\n$t")

        _         <- logger.info("Creating processor")
        processor <- SemiGlobalProcessor.make[F](s, t)
        _         <- logger.info(s"Building matrix")
        _         <- processor.build()
        _         <- logger.info(s"Building alignment")
        (_s, _t)  <- processor.alignment()
        _         <- logger.info(s"\n${_s}\n${_t}")
      } yield ()
    }
}
