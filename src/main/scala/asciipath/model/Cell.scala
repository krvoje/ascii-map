package asciipath.model

case class Cell(x: Int, y: Int, char: Char) {
  val isLetter        : Boolean = char.isLetter && char.isUpper
  val isStart         : Boolean = char == Cell.Start
  val isEnd           : Boolean = char == Cell.End
  val isCrossroads    : Boolean = char == Cell.Crossroads
  val isRoadVertical  : Boolean = char == Cell.RoadVertical
  val isRoadHorizontal: Boolean = char == Cell.RoadHorizontal
  val isRoad          : Boolean = isCrossroads || isRoadVertical || isRoadHorizontal

  val isPathSymbol:     Boolean = isLetter || isStart || isEnd || isRoad

  val hasVerticalDirection  : Boolean = isStart || isLetter || isCrossroads || isRoadVertical
  val hasHorizontalDirection: Boolean = isStart || isLetter || isCrossroads || isRoadHorizontal
}

object Cell {
  val Start          = '@'
  val End            = 'x'
  val Crossroads     = '+'
  val RoadVertical   = '|'
  val RoadHorizontal = '-'
}