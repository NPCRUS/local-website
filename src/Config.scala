import Scrapper.Target
import Utils.sitesFolderPath
import io.circe.{Codec, Decoder, Encoder}

import java.io.File
import java.net.URI
import java.nio.file.Path

given Codec[Path] = Codec.from(Decoder.decodeString.map(str => Path.of(str)), Encoder.encodeString.contramap(_.toString))

case class Config(name: String, siteSaveFrom: URI) derives Codec {
  
  lazy val localEntrypoint: URI = Target.Page(siteSaveFrom).localPath(using this).toUri
  
  lazy val siteRoot: URI = new URI(siteSaveFrom.getScheme, siteSaveFrom.getAuthority, null, null, null).resolve("/")
  
  lazy val localRootPath: Path = sitesFolderPath.resolve(name)
  
  lazy val configFile: File = localRootPath.resolve("config.json").toFile
  
  lazy val stateFile: File = localRootPath.resolve("state.json").toFile
  
  def init(): Unit = {
    localRootPath.toFile.mkdirs()
    configFile.createNewFile()
    stateFile.createNewFile()
  }
}
