package homework3

import java.util.concurrent.ForkJoinPool

import homework3.http.AsyncHttpClient
import homework3.math.Monoid
import homework3.processors.{BrokenLinkDetector, FileOutput, WordCounter}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object SpideyApp {
  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(new ForkJoinPool)
  val blockingExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(new ForkJoinPool(4))

  val httpClient = new AsyncHttpClient
  val spidey = new Spidey(httpClient)

  val usageInfo = """
                    |Usage:
                    |
                    |SpideyApp <url> <max-depth> <processor> [processor-config]
                    |
                    |Possible processors and their config are:
                    |
                    |file-output <target-dir>
                    |word-counter
                    |broken-link-detector
                  """.stripMargin

  def main(args: Array[String]): Unit = {
    val result = args match {
      case Array(url, maxDepth, "file-output", targetDir) => spidey.crawl(url, SpideyConfig(maxDepth.toInt))(new FileOutput(targetDir)(blockingExecutionContext))
      case Array(url, maxDepth, "word-counter") => spidey.crawl(url, SpideyConfig(maxDepth.toInt))(WordCounter)
      case Array(url, maxDepth, "broken-link-detector") => spidey.crawl(url, SpideyConfig(maxDepth.toInt))(BrokenLinkDetector)
      case _ => Future.successful(usageInfo)
    }

    println(Await.result(result, Duration.Inf))

    httpClient.shutdown()
  }
}
