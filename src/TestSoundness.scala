import org.jsoup.*
import gossamer.*
import telekinesis.*
import Utils.*
import nettlesome.*
import contingency.strategies.throwUnsafely

object TestSoundness extends App {
  
  val url = url"https://jsoup.org/cookbook"
  val url2 = url"https://jsoup.org/coocbook/index"
  println(url)
  println(url2)
  // println(url2.relativeTo(url))
}
