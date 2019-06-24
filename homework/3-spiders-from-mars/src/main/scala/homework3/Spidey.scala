package homework3

import homework3.html.HtmlUtils
import homework3.http._
import homework3.math.Monoid

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

case class SpideyConfig(maxDepth: Int,
                        sameDomainOnly: Boolean = true,
                        tolerateErrors: Boolean = true,
                        retriesOnError: Int = 0)

class Spidey(httpClient: HttpClient)(implicit ex: ExecutionContext) {
  private def sum[O: Monoid](xs: List[O]): O = {
    import Monoid.ops._
    xs.foldLeft(Monoid[O].identity)(_ |+| _)
  }

  private def linksOf(url: String, response: HttpResponse): List[String] = response.contentType match {
    case Some(contentType) if contentType.mimeType == ContentType.Html => HtmlUtils.linksOf(response.body, url)
    case _ => List.empty[String]
  }

  private def crawlLevel[O: Monoid](urls: List[String], level: Int, config: SpideyConfig)(processor: Processor[O]) = {
    val responses = urls.map(url => Await.result(httpClient.get(url), Duration.Inf))
    val urlsWithResponses = urls.zip(responses)
    val processedResponses = urlsWithResponses.map(p => processor(p._1, p._2))
    val nextLevelUrls = if (config.maxDepth > level) urlsWithResponses.flatMap(p => if (p._2.isSuccess) linksOf(p._1, p._2) else List.empty[String]) else List.empty[String]
    (nextLevelUrls, sum(processedResponses))
  }

  def crawl[O: Monoid](url: String, config: SpideyConfig)(processor: Processor[O]): Future[O] = {
    val maxDepth = config.maxDepth + 1
    @tailrec
    def crawlRec(urls: List[String], currentLevel: Int, acc: List[Future[O]]): Future[O] = currentLevel match {
      case `maxDepth` => sum(acc.reverse)
      case level =>
        val (nextLevelUrls, processedResult) = crawlLevel(urls, level + 1, config)(processor)
        crawlRec(nextLevelUrls.filter(HttpUtils.isValidHttp).diff(urls).distinct, level + 1, processedResult :: acc)
    }

    crawlRec(url :: Nil, 0, Nil)
  }
}
