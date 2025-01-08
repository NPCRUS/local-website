import java.net.URI

object TestRunner extends App {
  Scrapper.run(Config("jsoup", URI("https://jsoup.org/cookbook")))
  // Scrapper.run(Config("mill", URI("https://mill-build.org/mill/")))
}
