package homework3.processors

import homework3.Processor
import homework3.html.HtmlUtils
import homework3.http.{ContentType, HttpResponse}
import homework3.math.Monoid

import scala.concurrent.Future

case class WordCount(wordToCount: Map[String, Int])

object WordCount {
  def wordsOf(text: String): List[String] = text.split("\\W+").toList.filter(_.nonEmpty)

  implicit val wordCountMonoid: Monoid[WordCount] = new Monoid[WordCount] {

    import Monoid.ops._

    override def op(a: WordCount, b: WordCount): WordCount = WordCount(a.wordToCount |+| b.wordToCount)

    override def identity: WordCount = WordCount(Monoid[Map[String, Int]].identity)
  }
}

object WordCounter extends Processor[WordCount] {

  import homework3.SpideyApp.executionContext

  def apply(url: String, response: HttpResponse): Future[WordCount] = Future {
    (response.isSuccess, response.contentType) match {
      case (true, Some(contentType))
        if contentType.mimeType == ContentType.Html || contentType.mimeType == ContentType.PlainText => WordCount {
          WordCount.wordsOf(
            HtmlUtils.toText(response.body)
          ).foldLeft(Map.empty[String, Int]) { (acc, word) => acc + (word -> (acc.getOrElse(word, 0) + 1)) }
        }
      case _ => WordCount(Map.empty[String, Int])
    }
  }
}
