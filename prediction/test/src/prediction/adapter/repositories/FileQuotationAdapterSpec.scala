package prediction.adapter.repositories

import cats.effect._

import munit._

import prediction.domain.entities.quotation._

import java.nio.file._

final class FileQuotationAdapterSpec extends CatsEffectSuite {

  private val tmpPath: Resource[IO, Path] = {
    val acquire             = IO(Files.createTempFile("BaseFileSpec", ".tmp"))
    def release(path: Path) = IO(path.toFile.delete).attempt.void
    Resource.make(acquire)(release)
  }

  test("should read all bytes delegating to pipe") {
    val file =
      tmpPath.evalTap(file => IO(Files.write(file, Array[Byte](0, 1, 2, 3))))

    val pipe: fs2.Pipe[IO, Byte, Quotation] =
      stream => stream.map(_ => null) // Count each byte as 1 quotation

    val result = file.use(path => new FileQuotationAdapter[IO](path, pipe).load())

    assertIO(result, Vector.fill(4)(null))
  }
}
