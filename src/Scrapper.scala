import Scrapper.Target.Page
import Utils.*
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import org.jsoup.*
import org.jsoup.nodes.Document

import java.io.{File, FileOutputStream, FileWriter}
import java.net.URI
import java.nio.file.Path
import scala.annotation.tailrec
import scala.io.{Source, StdIn}
import scala.util.Try

object Scrapper {

  case class State(queue: Seq[Target], processedUrls: Set[URI]) derives Codec

  object Target {
    given Ordering[Target] with
      override def compare(x: Target, y: Target): Int = (x, y) match
        case (_: Resource, _: Page) => -1
        case (_: Page, _: Resource) => 1
        case _ => 0
  }
  enum Target(val url: URI) derives Codec {
    case Page(override val url: URI) extends Target(url)
    case Resource(override val url: URI, attr: String) extends Target(url)

    def localPath(using config: Config): Path =
      val relative = config.siteRoot.path.relativize(url.path)
      addIndex(config.localRootPath.resolve(relative))
  }

  def savePage(target: Target, document: Option[Document])(using config: Config) = {
    val filePath = target.localPath
    val file = filePath.toFile
    println(s"Saving ${target.url}")
    // make dirs hierarchy
    filePath.resolve("..").toFile.mkdirs()
    document match
      case None =>
        val inputStream = target.url.toURL.openStream()
        val outputStream = new FileOutputStream(file)
        inputStream.transferTo(outputStream)
      case Some(value) =>
        val doc = value.toString
        val writer = FileWriter(file)
        writer.write(doc)
        writer.close()
  }

  def processTarget(target: Target)(using config: Config): Seq[Target] = target match
    case Target.Page(url) =>
      val connection = Jsoup.connect(url.toString).followRedirects(true)
      val parsedPage = connection.get()
      val elements = toIterable(parsedPage.getElementsByAttribute("href")).toSeq
      val scripts = toIterable(parsedPage.getElementsByTag("script")).toSeq
      val images = toIterable(parsedPage.getElementsByTag("img")).toSeq
      val targets = (elements ++ scripts ++ images)
        .map { element =>
          val target = if (element.is("a")) {
            val url = URI(element.attr("abs:href"))
            Some(Target.Page(url))
          } else if (element.is("link") && element.attr("rel") == "stylesheet") {
            val url = URI(element.attr("abs:href"))
            Some(Target.Resource(url, "href"))
          } else if ((element.is("script") || element.is("img")) && element.hasAttr("src")) {
            val url = URI(element.attr("abs:src"))
            Some(Target.Resource(url, "src"))
          } else {
            // println(s"skip $element")
            None
          }

          // change the element link to new local absolute
          target match
            case Some(p: Target.Page) if config.siteSaveFrom.isBaseFor(p.url) =>
              element.attr("href", p.localPath.toString)
            case Some(r: Target.Resource) if config.siteRoot.isBaseFor(r.url) =>
              element.attr(r.attr, r.localPath.toString)
            case _ => ()

          target
        }
        .collect { case Some(v) =>
          v
        }.map {
          case Target.Page(url) =>
            Target.Page(URI(url.toString.split("#")(0)))
          case other => other
        }
      savePage(target, Some(parsedPage))
      targets
    case Target.Resource(url, _) =>
      savePage(target, None)
      if(url.toString.endsWith("css")) {
        val regex = "url\\(['\"]*(.*?)['\"]*\\)".r
        val bytes = target.url.toURL.openStream().readAllBytes()
        val css = String(bytes)
        val res = regex.findAllMatchIn(css).toSeq.map(_.group(1))
          .map(URI.create)
          .map {
            case uri if uri.isAbsolute => uri
            case uri => url.resolve(uri)
          }
          .map(Target.Resource(_, "href"))
        res
      } else {
        Seq.empty
      }

  private def saveState(stateFile: File, state: State): Unit =
    val writer = FileWriter(stateFile)
    writer.write(state.asJson.spaces2)
    writer.close()

  def run(config: Config): Unit = {
    config.init()

    // save config
    val writer = FileWriter(config.configFile)
    writer.write(config.asJson.spaces2)
    writer.close()

    // init state
    val stateSource = Source.fromFile(config.stateFile)
    val maybeState = parse(stateSource.getLines.mkString("\n"))
      .flatMap(_.as[State]).toOption
    stateSource.close()

    val rootTarget = Target.Page(config.siteSaveFrom)
    val state = maybeState match
      case Some(value) =>
        println("do you want to continue from last time? Y/n")
        val answer = StdIn.readLine()
        if (answer == "y" || answer == "Y") {
          value
        } else {
          State(Seq(rootTarget), Set.empty)
        }
      case None => State(Seq(rootTarget), Set.empty)

    @tailrec
    def fold(state: State, config: Config): State =
      state.queue match {
        case Seq() => state
        case target +: rest =>
          println(
            s"Processing ${state.processedUrls.size + 1} out of ${state.queue.length + state.processedUrls.size}"
          )
          val newState = State(rest, state.processedUrls + target.url)
          val processResult = Try(processTarget(target)(using config)).recover { case e: Throwable =>
            println(s"failed processing $target")
            saveState(config.stateFile, state)
            throw e
          }.get
          val newTargets = processResult
            .distinctBy(_.url)
            .filterNot(t => newState.processedUrls.contains(t.url))
            .filterNot(t => state.queue.exists(_.url == t.url))
            .filter {
              case p: Target.Page if config.siteSaveFrom.isBaseFor(p.url) =>
                true
              case r: Target.Resource if config.siteSaveFrom.isBaseFor(r.url) => true
              case _ => false
            }
          fold(
            newState.copy(queue = (rest ++ newTargets).sorted),
            config
          )
      }

    val result = fold(state, config)
    config.stateFile.delete()
    println("AllProcessedUrls:")
    println(result.processedUrls.mkString("\n"))
  }
}
