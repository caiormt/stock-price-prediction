package prediction

import cats.effect._

import io.chrisdavenport.log4cats.slf4j._

import prediction.algebras._

import java.net.URL
import java.nio.file.Paths

object App extends IOApp {

  import fs2._

  final val codNeg: String =
    "BOVA11"

  final val path: URL =
    App.getClass.getClassLoader.getResource(s"$codNeg.TXT")

  override def run(args: List[String]): IO[ExitCode] =
    program[IO].as(ExitCode.Success)

  private def program[F[_]: Concurrent: ContextShift]: F[Unit] =
    Stream
      .eval(Slf4jLogger.create[F])
      .flatMap { implicit logger =>
        for {
          _         <- Stream.eval(logger.info("Warming up..."))
          reader    <- Stream.eval(B3Reader.make[F])
          extractor <- Stream.eval(B3Extractor.make[F])
          _         <- Stream.eval(logger.info("Start processing..."))
          data       = reader.fromFile(Paths.get(path.toURI()))
          sequence  <- Stream.eval(extractor.toAlphabet(data))
          _         <- Stream.eval(logger.info(s"Sequence extracted: $sequence"))
          s          = sequence.substring(0, 10)
          processor <- Stream.eval(LiveProcessor.make[F](s, s))
          _         <- Stream.eval(processor.build())
          matrix    <- Stream.eval(processor.getMatrix())
          _         <- Stream.eval(logger.info(s"\n${matrix.toString()}"))
        } yield ()
      }
      .compile
      .drain
}
