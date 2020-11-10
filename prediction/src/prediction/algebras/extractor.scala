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
  def make[F[_]: Sync]: F[Extractor[F, Entry]] =
    Sync[F].delay(new B3Extractor[F])
}

final class B3Extractor[F[_]: Sync] extends Extractor[F, Entry] {
  override def toAlphabet(data: Stream[F, Entry]): F[String] =
    data
      .collect { case r: Register => r }
      .map {
        case r if r.preAbe < r.preUlt  => Alphabet.P
        case r if r.preAbe == r.preUlt => Alphabet.Z
        case r if r.preAbe > r.preUlt  => Alphabet.N
      }
      .widen[Alphabet]
      .map(_.show)
      .compile
      .string
}
