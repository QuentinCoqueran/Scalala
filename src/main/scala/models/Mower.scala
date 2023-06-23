package models

case class Mower(position: Position, lawn: Lawn, startX: Int, startY: Int) {
  def getX: Int = startX
  def getY: Int = startY

  def rotateRight: Mower =
    copy(position =
      position.copy(orientation = rotateOrientationRight(position.orientation))
    )

  def rotateLeft: Mower =
    copy(position =
      position.copy(orientation = rotateOrientationLeft(position.orientation))
    )

  def moveForward: Mower = {
    val newPosition = calculateNewPosition()
    if (lawn.isValidPosition(newPosition))
      copy(position = newPosition)
    else
      this
  }

  def rotateOrientationRight(orientation: Orientation): Orientation =
    orientation match {
      case North => East
      case East  => South
      case South => West
      case West  => North
    }

  def rotateOrientationLeft(orientation: Orientation): Orientation =
    orientation match {
      case North => West
      case East  => North
      case South => East
      case West  => South
    }

  def calculateNewPosition(): Position = position.orientation match {
    case North => position.copy(y = position.y + 1)
    case East  => position.copy(x = position.x + 1)
    case South => position.copy(y = position.y - 1)
    case West  => position.copy(x = position.x - 1)
  }
}
