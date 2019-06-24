package homework3.processors

import homework3.Processor
import homework3.http.HttpResponse

import scala.concurrent.Future

object BrokenLinkDetector extends Processor[Set[String]] {
  import homework3.SpideyApp.executionContext

  def apply(url: String, response: HttpResponse): Future[Set[String]] = Future {
    if (response.status == 404) Set(url)
    else Set.empty[String]
  }
}