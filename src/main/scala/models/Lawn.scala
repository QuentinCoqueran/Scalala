package models

case class Lawn(topRightX: Int, topRightY: Int) {
  def isValidPosition(position: Position): Boolean =
    position.x >= 0 && position.x <= topRightX &&
      position.y >= 0 && position.y <= topRightY
}
