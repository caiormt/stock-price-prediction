package prediction.algebras

import cats.implicits._

import cats.effect._

import fs2._

import prediction.domain._
import prediction.domain.b3._

trait Extractor[F[_], A] {
  def toAlphabet(data: Stream[F, A]): F[String]
}

object B3Extractor {
  def make[F[_]: Sync]: F[Extractor[F, Register]] =
    Sync[F].delay(new B3Extractor[F])
}

final class B3Extractor[F[_]: Sync] extends Extractor[F, Register] {
  override def toAlphabet(data: Stream[F, Register]): F[String] =
    data.map(registerToAlphabet).map(_.show).compile.string

  private val registerToAlphabet: Register => Alphabet = {
    case r if r.preAbe < r.preUlt => Alphabet.P
    case r if r.preAbe > r.preUlt => Alphabet.N
    case _                        => Alphabet.Z
  }
}
