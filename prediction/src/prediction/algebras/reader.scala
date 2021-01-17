package prediction.algebras

import atto.Atto._
import atto.fs2.Pipes._

import cats.implicits._

import cats.effect._

import fs2._

import prediction.domain.b3._
import prediction.parser.b3._

import java.nio.file.Path

trait Reader[F[_], A] {
  def fromFile(path: Path): Stream[F, A]
}

object B3Reader {
  def make[F[_]: Sync: ContextShift]: F[Reader[F, Register]] =
    Sync[F].delay(new B3Reader[F](chunkSize = 1024 * 32))
}

final class B3Reader[F[_]: Sync: ContextShift] private (private val chunkSize: Int) extends Reader[F, Register] {
  override def fromFile(path: Path): Stream[F, Register] =
    Stream.resource(Blocker[F]).flatMap { blocker =>
      io.file
        .readAll[F](path, blocker, chunkSize)
        .through(text.utf8Decode)
        .through(text.lines)
        .through(parseLenient(choice(header, register, trailer)))
        .collect { case r: Register => r }
    }
}
