package models

case class Position(x: Int, y: Int, orientation: Orientation)

sealed trait Orientation

case object North extends Orientation

case object East extends Orientation

case object South extends Orientation

case object West extends Orientation
