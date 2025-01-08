import io.circe.parser.*

import java.net.URI
import java.awt.Desktop
import scala.io.Source
import scala.util.{Failure, Success}

object Main {
  @main
  def entrypoint(args: String*): Unit =
  args.toList match {
    case "save" :: name :: url :: Nil =>
      val config = Config(name, URI(url))
      Scrapper.run(config)
    case "open" :: name :: Nil =>
      val siteFolder = Utils.sitesFolderPath.resolve(name)
      if(!siteFolder.toFile.exists()) {
        println(s"Nothing saved with such identifier: $name")
      } else {
        val configFile = siteFolder.resolve("config.json").toFile
        val source = Source.fromFile(configFile)
        val config = parse(source.getLines().mkString("\n")).flatMap(_.as[Config]).toTry match
          case Failure(exception) =>
            source.close()
            println("cannot read config file: ")
            throw exception
          case Success(value) => value
        source.close()


        val desktop = Desktop.getDesktop
        desktop.browse(config.localEntrypoint)
      }
    case _ =>
      println(s"unknown sequence: \"${args.mkString(" ")}\"")
  }
}
