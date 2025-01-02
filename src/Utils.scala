import java.nio.file.Path
import java.net.URI
import java.util

object Utils {
  
  def toIterable[T](v: util.ArrayList[T]): IterableOnce[T] = new IterableOnce[T] {
    def iterator: Iterator[T] = Iterator.range(0, v.size()).map(v.get)
  }
  
  def addIndex(relativePath: Path) =
    if (relativePath.getFileName.toString.contains(".")) {
      relativePath
    } else {
      relativePath.resolve("index.html")
    }

  extension (uri: URI) {
    def isBaseFor(other: URI) = {
      if (uri.getHost != other.getHost) {
        false
      } else {
        val rootPath = uri.path
        val path = other.path
        !rootPath.relativize(path).startsWith("..")
      }
    }

    def path: Path = {
      val path = Path.of(uri.getPath)
      if (path.toString == "") {
        Path.of(uri.getPath + "/")
      } else {
        path
      }
    }
  }
}
