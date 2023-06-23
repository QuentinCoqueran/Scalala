package progfun

import better.files.File
import com.typesafe.config.{Config, ConfigFactory}
import play.api.libs.json.Json
import models._
import scala.util.Try

trait IParserDownload {
  def createFileOutput(
      finalPositions: List[
        (Position, Orientation, List[Instruction], (Int, Int))
      ],
      lawn: Lawn): Unit
}

case class ParseFile() {
  def createParser(): List[IParserDownload] = {
    // execute new JsonParser, new CsvCommandParser, new YamlCommandParser
    List(new JsonParser, new CsvCommandParser, new YamlCommandParser)
  }

  private def instructionToChar(instruction: Instruction): String =
    instruction match {
      case RotateRight => "D"
      case RotateLeft  => "G"
      case MoveForward => "A"
    }

  private class JsonParser extends IParserDownload {
    def createFileOutput(
        finalPositions: List[
          (Position, Orientation, List[Instruction], (Int, Int))
        ],
        lawn: Lawn): Unit = {

      val json = Json.obj(
        "limite" -> Json.obj(
          "x" -> lawn.getX,
          "y" -> lawn.getY
        ),
        "tondeuses" -> finalPositions.map {
          case (position, orientation, instructions, startPositionMower) =>
            Json.obj(
              "debut" -> Json.obj(
                "point" -> Json.obj(
                  "x" -> startPositionMower._1,
                  "y" -> startPositionMower._2
                ),
                "direction" -> orientation.toString
              ),
              "instructions" -> instructions.map(instructionToChar),
              "fin" -> Json.obj(
                "point"     -> Json.obj("x" -> position.x, "y" -> position.y),
                "direction" -> orientation.toString
              )
            )
        }
      )

      val conf: Config = ConfigFactory.load()
      val outputFile = File(conf.getString("application.output-json-file"))
      val result = Try(outputFile.write(json.toString()))
      result match {
        case scala.util.Success(_) =>
          println("Fichier JSON de sortie généré avec succès")
        case scala.util.Failure(_) =>
          println("Erreur lors de la génération du fichier JSON de sortie")
      }
    }
  }

  private class CsvCommandParser extends IParserDownload {
    def createFileOutput(
        finalPositions: List[
          (Position, Orientation, List[Instruction], (Int, Int))
        ],
        lawn: Lawn): Unit = {
      val conf: Config = ConfigFactory.load()
      val outputFile = File(conf.getString("application.output-csv-file"))

      val lines = finalPositions.zipWithIndex.map {
        case (
              (
                position,
                orientation,
                instructions,
                startPositionMower: (Int, Int)
              ),
              index
            ) =>
          val line: String =
            s"${(index + 1).toString};${startPositionMower._1.toString};${startPositionMower._2.toString};" +
              s"${orientation.toString};${position.x.toString};${position.y.toString};${orientation.toString};" +
              s"${instructions.map(instructionToChar).mkString("")}"
          line
      }

      val csvContent =
        ("numéro;début_x;début_y;début_direction;fin_x;fin_y;fin_direction;instructions" +: lines)
          .mkString("\n")
      val result = Try(outputFile.write(csvContent))
      result match {
        case scala.util.Success(_) =>
          println("Fichier CSV de sortie généré avec succès")
        case scala.util.Failure(_) =>
          println("Erreur lors de la génération du fichier CSV de sortie")
      }
    }
  }

  private class YamlCommandParser extends IParserDownload {
    def createFileOutput(
        finalPositions: List[
          (Position, Orientation, List[Instruction], (Int, Int))
        ],
        lawn: Lawn): Unit = {
      val conf: Config = ConfigFactory.load()
      val outputFile = File(conf.getString("application.output-yaml-file"))

      val yamlContent = finalPositions
        .map {
          case
                (
                  position,
                  orientation,
                  instructions,
                  startPositionMower: (Int, Int)
                )
               =>
            val instructionLines = instructions
              .map(instructionToChar)
              .map(instruction => s"     - $instruction")
              .toString
              .replace("List(", "")
              .replace(")", "")
              .replace(",", "\n")

            s"""  - debut:
             |      point:
             |        x: ${startPositionMower._1.toString}
             |        y: ${startPositionMower._2.toString}
             |      direction: ${orientation.toString}
             |    instructions:
             | $instructionLines
             |    fin:
             |      point:
             |        x: ${position.x.toString}
             |        y: ${position.y.toString}
             |      direction: ${orientation.toString}
             |""".stripMargin
        }
        .mkString("\n")

      val yamlOutput =
        s"""limite:
           |  x: ${lawn.getX.toString}
           |  y: ${lawn.getY.toString}
           |tondeuses:
           |$yamlContent
           |""".stripMargin

      val result = Try(outputFile.write(yamlOutput))
      result match {
        case scala.util.Success(_) =>
          println("Fichier YAML de sortie généré avec succès")
        case scala.util.Failure(_) =>
          println("Erreur lors de la génération du fichier YAML de sortie")
      }
    }
  }
}
