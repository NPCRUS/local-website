import soundness.*
import contingency.strategies.throwUnsafely

import java.io.File
import java.net.URI

object TestSoundness extends App {

  // testing glob patterns
  val g = Glob.parse("../api")
  println(g)
  val gUrl1 = "https://mill.org/mill/0.12.9/api/index.html".tt
  val gUrl2 = "https://mill.org/mill/index.html".tt
  val gRes = gUrl2 match {
    case g"**/api/**" => true
    case _ => false
  }
  println(gRes)

  val file = new File("/Users/nikitaglushchenko/projects/save/sites/mill/_/css/site.css")
  val source = scala.io.Source.fromFile(file)
  // val css = source.getLines().mkString("\n")
  val css = "@font-face{font-family:Roboto;font-style:normal;font-weight:400;src:url(../font/roboto-latin-400-normal.woff2)"
  source.close()

  val result = css.tt match {
    case r"url\($url(.*?)\)" => Some(url)
    case _ => None
  }
  println(result)

  val url2 = url"https://a.com"
  val url3 = url"https://a.com/b"
  // what does relativeTo return?
  // what methods available on Relative
  // when navigating inside the method
  // - code is barely readable
  // throws runtime error
  val relative = url3.relativeTo(url2)
  assert(relative.delta == 1)





  val url1 = url"https://google.com/#title"
  assert(url1.toString() == "https://google.com/#title")
}
