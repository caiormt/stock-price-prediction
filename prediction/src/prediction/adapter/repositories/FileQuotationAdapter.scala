package prediction.adapter.repositories

import cats.effect._

import fs2._
import fs2.io.file._

import prediction.domain.entities.quotation._

import prediction.data.ports.repositories._

import natchez._

import java.nio.file.Path

final class FileQuotationAdapter[F[_]: Async: Trace](
    path: Path,
    parser: Pipe[F, Byte, Quotation],
    chunkSize: Int = 4096
) extends QuotationRepositoryPort[F] {

  override def load(): F[Vector[Quotation]] =
    Trace[F].span("file-quotation-load") {
      Files[F].readAll(path, chunkSize).through(parser).compile.toVector
    }
}
