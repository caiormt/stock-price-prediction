package prediction.adapter.atto

import atto.{ ParseResult, Parser }

import fs2._

// $COVERAGE-OFF$
object pipes {

  /* Ignoring coverage here, just code copied from atto.fs2.Pipes
   * because fs2 upgrade from version 2 to 3 is binary incompatible
   * and atto does not have support yet.
   */
  def parseN[F[_], A](p: Parser[A]): Pipe[F, String, A] = s => {
    def exhaust(r: ParseResult[A], acc: List[A]): (ParseResult[A], List[A]) =
      r match {
        case ParseResult.Done(in, a) => exhaust(Parser.parse(p, in), a :: acc)
        case _                       => (r, acc)
      }

    def go(r: ParseResult[A])(s: Stream[F, String]): Pull[F, A, Unit] =
      s.pull.uncons1.flatMap {
        case Some((s, rest)) =>
          val (r0, acc) = r match {
            case ParseResult.Done(in, a)   => (Parser.parse(p, in + s), List(a))
            case ParseResult.Fail(_, _, _) => (r, Nil)
            case ParseResult.Partial(_)    => (r.feed(s), Nil)
          }
          val (r1, as)  = exhaust(r0, acc)
          Pull.output(Chunk.seq(as.reverse)) >> go(r1)(rest)
        case None            => Pull.output(Chunk.seq(exhaust(r.done, Nil)._2))
      }

    go(Parser.parse(p, ""))(s).stream
  }
}
// $COVERAGE-ON$
