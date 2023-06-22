package progfun

import better.files.File
import com.typesafe.config.{Config, ConfigFactory}
import scala.util.{Failure, Success, Try}
object UploadFileParser extends App {
  val conf: Config = ConfigFactory.load()
  private val inputFile = conf.getString("application.input-file")
  private val result: Try[Unit] = Try {
    val f = File(inputFile)
    for (line <- f.lines.toList) {
      println(line)
    }
  }
  result match {
    case Success(_) =>
        println("Le fichier a été traité avec succès")
        System.exit(0)
    case Failure(ex) =>
      println(s"Une erreur s'est produite : ${ex.getMessage}")
      System.exit(1)
  }
}
