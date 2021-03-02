package prediction.data.usecases
package b3

import cats.implicits._

import cats.effect._

import weaver._
import weaver.scalacheck._

import prediction.domain.entities._
import prediction.domain.entities.algorithm._

object B3AlgorithmParserSpec extends SimpleIOSuite with Checkers {
  test("Equal opening and closing price") {
    forall(equalQuotationGen) { quotation =>
      for {
        parser <- B3AlgorithmParser.make[IO]
        token  <- parser.parseQuotation(quotation)
      } yield expect(token === AlgorithmToken(Alphabet.Draw))
    }
  }

  test("Opening less than closing price") {
    forall(lessQuotationGen) { quotation =>
      for {
        parser <- B3AlgorithmParser.make[IO]
        token  <- parser.parseQuotation(quotation)
      } yield expect(token === AlgorithmToken(Alphabet.Negative))
    }
  }

  test("Opening greater than closing price") {
    forall(greaterQuotationGen) { quotation =>
      for {
        parser <- B3AlgorithmParser.make[IO]
        token  <- parser.parseQuotation(quotation)
      } yield expect(token === AlgorithmToken(Alphabet.Positive))
    }
  }
}
