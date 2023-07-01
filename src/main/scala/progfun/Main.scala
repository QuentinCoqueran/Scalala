package progfun

import better.files.File
import com.typesafe.config.{Config, ConfigFactory}
import models._

import scala.util.Try

sealed trait Instruction

case object RotateRight extends Instruction

case object RotateLeft extends Instruction

case object MoveForward extends Instruction

object Main {
  def main(args: Array[String]): Unit = {
    val conf: Config = ConfigFactory.load()
    val inputFile = File(conf.getString("application.input-file"))
    val lines = inputFile.lines.toList
    // Parsing de la taille de la pelouse
    // flatMap applique une fonction à un élément et retourne le résultat dans un conteneur
    // :: indique le format qu'on recherche 2
    val lawnSize = lines.headOption
      .flatMap(line =>
        line.split(" ").toList match {
          // Tuple
          case x :: y :: Nil => Some((x.toInt, y.toInt))
          case _             => None
        }
      )
      .toRight(new IllegalArgumentException("Invalid lawn size"))
    // fold permet de gérer les cas d'erreur
    val lawn = lawnSize.fold(
      error => {
        // Gestion de l'erreur de taille de pelouse
        println(error.getMessage)
        sys.exit(1)
      },
      // sinon on crée la pelouse
      // _1 et _2 sont des accesseurs pour extraire respectivement le premier et le deuxième élément d'un tuple.
      size => Lawn(size._1, size._2)
    )

    // Parsing des lignes correspondant aux tondeuses
    // on supprime la premiere ligne et on regroupe les lignes par 2
    val mowerLines = lines.drop(1).grouped(2).toList
    val mowers = mowerLines.flatMap {
      case positionLine :: instructionLine :: Nil =>
        // for et yield genere une nouvelle liste dans position et instructions
        for {
          position     <- parsePosition(positionLine)
          instructions <- parseInstructions(instructionLine)
        } yield (position, instructions)
      case _ => None
    }

    ParseFile()
      .createParser()
      .foreach(
        _.createFileOutput(
          executeInstructions(
            lawn,
            mowers
          ),
          lawn
        )
      )
  }
  private def parsePosition(line: String): Option[Position] = {
    val parts = line.split(" ")
    if (parts.length == 3) {
      val x = Try(parts(0).toInt)
      val y = Try(parts(1).toInt)
      val orientation: Option[Orientation] = parts(2) match {
        case "N" => Some(North)
        case "E" => Some(East)
        case "S" => Some(South)
        case "W" => Some(West)
        case _   => None
      }
      // toOption returns Some if the value is non-null, otherwise None
      // if x or y is None, the result is None and the for comprehension will fail
      for {
        xPos   <- x.toOption
        yPos   <- y.toOption
        orient <- orientation
      } yield Position(xPos, yPos, orient)
    } else {
      None
    }
  }

  private def parseInstructions(line: String): Option[List[Instruction]] = {
    // on utilise seq pour map
    val instructions: Seq[Option[Instruction]] = line.map {
      case 'G' => Some(RotateLeft)
      case 'D' => Some(RotateRight)
      case 'A' => Some(MoveForward)
      case _   => None
    }
    if (instructions.contains(None)) None
    else Some(instructions.flatten.toList)
  }

  private def executeInstructions(
      lawn: Lawn,
      instructions: List[(Position, List[Instruction])]
  ): List[(Position, Orientation, List[Instruction], (Int, Int))] = {
    instructions.map { case (position, instructions) =>
      val mower = Mower(position, lawn, position.x, position.y)
      // foldLeft prend en compte l'instruction actuelle et met à jour la tondeuse en conséquence
      val finalMower = instructions.foldLeft(mower) { (m, instruction) =>
        instruction match {
          case RotateRight => m.rotateRight
          case RotateLeft  => m.rotateLeft
          case MoveForward => m.moveForward
        }
      }
      val startPositionMower = (
        finalMower.getX,
        finalMower.getY
      )
      (
        finalMower.position,
        finalMower.position.orientation,
        instructions,
        startPositionMower
      )
    }
  }
}
