package prediction

import cats.implicits._

import cats.effect._

import io.chrisdavenport.log4cats.slf4j._

import prediction.algebras._

import java.nio.file.Paths
import java.{ time => jt }

object App extends IOApp {

  final val formatter: jt.format.DateTimeFormatter =
    jt.format.DateTimeFormatter.ISO_DATE

  override def run(args: List[String]): IO[ExitCode] =
    program[IO](args)
      .unlessA(args.size =!= 3)
      .as(ExitCode.Success)

  private def program[F[_]: Concurrent: ContextShift](args: List[String]): F[Unit] =
    Slf4jLogger.create[F].flatMap { logger =>
      for {
        _     <- logger.info("Parsing arguments")
        path  <- Sync[F].delay(args(0))
        begin <- Sync[F].delay(jt.LocalDate.parse(args(1), formatter))
        end   <- Sync[F].delay(jt.LocalDate.parse(args(2), formatter))

        _         <- logger.info("Warming up")
        reader    <- B3Reader.make[F]
        extractor <- B3Extractor.make[F]

        _   <- logger.info("Start processing...")
        data = reader.fromFile(Paths.get(path))

        data0 = data.filter(_.date.isBefore(begin))
        s    <- extractor.toAlphabet(data0)
        _    <- logger.info(s"Long sequence: $s")

        data1 = data.filter(e => e.date.isAfter(begin) && e.date.isBefore(end))
        t    <- extractor.toAlphabet(data1)
        _    <- logger.info(s"Short sequence: $t")

        processor <- SemiGlobalProcessor.make[F](s, t)
        _         <- logger.info(s"Building matrix")
        _         <- processor.build()
        _         <- logger.info(s"Building alignment")
        (_s, _t)  <- processor.alignment()
        _         <- logger.info(s"\n${_s}\n${_t}")
      } yield ()
    }
}
