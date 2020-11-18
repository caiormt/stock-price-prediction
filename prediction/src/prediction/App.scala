package prediction

import cats._
import cats.implicits._

import cats.effect._

import io.chrisdavenport.log4cats._
import io.chrisdavenport.log4cats.slf4j._

import prediction.algebras._

import java.net.URL
import java.nio.file.Paths

object App extends IOApp {

  final val codNeg: String =
    "BOVA11"

  final val path: URL =
    App.getClass.getClassLoader.getResource(s"$codNeg.TXT")

  override def run(args: List[String]): IO[ExitCode] =
    program[IO].as(ExitCode.Success)

  private def program[F[_]: Concurrent: ContextShift]: F[Unit] =
    Slf4jLogger.create[F].flatMap { implicit logger =>
      for {
        _         <- logger.info("Warming up...")
        reader    <- B3Reader.make[F]
        extractor <- B3Extractor.make[F]

        _        <- logger.info("Start processing...")
        data      = reader.fromFile(Paths.get(path.toURI()))
        sequence <- extractor.toAlphabet(data)
        _        <- logger.info(s"Sequence extracted: $sequence")
        s         = sequence.substring(0, 20)
        t         = sequence.substring(4, 8)

        _      <- Logger[F].info(s"Starting Global Processor")
        global <- GlobalProcessor.make[F](s, t)
        _      <- execute(global)

        _    <- Logger[F].info(s"Starting SemiGlobal Processor")
        semi <- SemiGlobalProcessor.make[F](s, t)
        _    <- execute(semi)

        _     <- Logger[F].info(s"Starting Local Processor")
        local <- LocalProcessor.make[F](s, t)
        _     <- execute(local)

      } yield ()
    }

  private def execute[F[_]: FlatMap: Logger](processor: Processor[F, String, Char, Long]): F[Unit] =
    for {
      _          <- processor.build()
      similarity <- processor.similarity()
      _          <- Logger[F].info(s"Similarity $similarity")
      matrix     <- processor.matrix()
      _          <- Logger[F].info(s"\n${matrix.toString()}")
      (s, t)     <- processor.alignment()
      _          <- Logger[F].info(s"\n$s\n$t")
    } yield ()
}
