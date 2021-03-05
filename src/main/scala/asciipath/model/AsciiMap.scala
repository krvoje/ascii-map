package asciipath.model

case class AsciiMap(cells: Set[Cell]) {
  lazy val start: Cell = cells.find(_.isStart).getOrElse(sys.error("Invalid map, no start point provided"))
  lazy val end  : Cell = cells.find(_.isEnd).getOrElse(sys.error("Invalid map, no end point provided"))

  lazy val width : Int = if (cells.isEmpty) 0 else cells.map(_.x).max
  lazy val height: Int = if (cells.isEmpty) 0 else cells.map(_.y).max

  def get(x: Int, y: Int): Option[Cell] = cells.find(it => it.x == x && it.y == y)

  def right(cell: Cell): Option[Cell] = get(cell.x + 1, cell.y)

  def left(cell: Cell): Option[Cell] = get(cell.x - 1, cell.y)

  def up(cell: Cell): Option[Cell] = get(cell.x, cell.y - 1)

  def down(cell: Cell): Option[Cell] = get(cell.x, cell.y + 1)

  def neighbour(cell: Cell, direction: Movement): Option[Cell] = direction match {
    case GoLeft => left(cell)
    case GoRight => right(cell)
    case GoUp => up(cell)
    case GoDown => down(cell)
    case _ => None
  }

  def neighboursExcluding(cell: Cell, exclude: Option[Movement] = None): Map[Movement, Cell] = {
    Map(
      GoLeft -> left(cell),
      GoRight -> right(cell),
      GoUp -> up(cell),
      GoDown -> down(cell),
      ).collect {
      case direction -> Some(cell) => direction -> cell
    }.filterNot {
      case direction -> cell => exclude.contains(direction)
    }
  }

  override def toString: String = {
    Range(0, height+1).map { row =>
      Range(0, width+1).map { col =>
        get(col, row).map(_.char).getOrElse(' ')
      }.mkString("")
    }.mkString("\n")
  }
}