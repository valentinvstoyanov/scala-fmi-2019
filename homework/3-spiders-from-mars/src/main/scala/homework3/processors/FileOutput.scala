package homework3.processors

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.util.UUID

import homework3.Processor
import homework3.http.HttpResponse
import homework3.math.Monoid

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class SavedFiles(urlToPath: Map[String, Path])

object SavedFiles {
  implicit val savedFilesMonoid = new Monoid[SavedFiles] {
    override def op(a: SavedFiles, b: SavedFiles) = SavedFiles(a.urlToPath ++ b.urlToPath)

    override def identity: SavedFiles = SavedFiles(Map.empty[String, Path])
  }
}

class FileOutput(targetDir: String)
                (ex: ExecutionContext) extends Processor[SavedFiles] {
  private implicit val blockingExc: ExecutionContext = ex

  private val targetPath = Paths.get(targetDir)

  private def generatePathFor(url: String): Path = {
    val urlFileName = Option(Paths.get(new URI(url).getPath).getFileName).map(_.toString).getOrElse("")
    val fileName = s"${UUID.randomUUID().toString}-$urlFileName"

    targetPath.resolve(fileName)
  }

  def apply(url: String, response: HttpResponse): Future[SavedFiles] = Future {
    if (response.isSuccess) {
      Try(Files.write(generatePathFor(url), response.bodyAsBytes)) match {
        case Success(path) => SavedFiles(Map(url -> path))
        case Failure(_) => SavedFiles(Map.empty[String, Path])
      }
    } else SavedFiles(Map.empty[String, Path])
  }
}
