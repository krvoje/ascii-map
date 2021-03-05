package asciipath

import asciipath.model.Movement
import asciipath.model._

import scala.collection.mutable.ListBuffer

class AsciiMapService(validator: AsciiMapValidator) {

  def parse(input: String): AsciiMap = {
    val lines = input.linesIterator.toSeq

    val result = ListBuffer[Cell]()
    for {
      (line, y) <- lines.zipWithIndex
      (char, x) <- line.toCharArray.zipWithIndex
      cell = Cell(x, y, char)
      if cell.isPathSymbol
    } {
      result += cell
    }
    AsciiMap(result.toSet)
  }

  def traverse(input: String): Either[InvalidAsciiMap, AsciiMapPath] = traverse(parse(input))

  /**
   * Traverses the map and returns the resulting path. Assumes a valid traversable map.
   * (Didn't do fancy stuff like cycle detection).
   */
  def traverse(asciiMap: AsciiMap): Either[InvalidAsciiMap, AsciiMapPath] = for {
    asciiMap <- validator.validate(asciiMap)
  } yield {
    val path               = ListBuffer[Cell]()
    var isEndFound         = false
    var current            = asciiMap.start
    var movement: Movement = asciiMap
      .neighboursExcluding(asciiMap.start)
      .keys
      .headOption
      .getOrElse(sys.error("Start must have a single neighbour"))
    // Could have made this a recursion, but I prefer a while loop since it gives a holistic overview
    while (!isEndFound) {
      path += current
      if (current.isEnd) isEndFound = true
      else if (current.isStart || current.isCrossroads || current.isLetter && asciiMap.neighbour(current, movement).isEmpty) {
        val movements = asciiMap.neighboursExcluding(current, exclude = Some(movement.opposite))
        val newDirection -> nextCell = movements.head
        movement = newDirection
        current = nextCell
      } else {
        current = asciiMap.neighbour(current, movement)
                          .getOrElse(sys.error(s"Invalid map, expected to find a neighbour of cell $current"))
      }
    }

    AsciiMapPath(
      letters = path.filter(_.isLetter).distinct.map(_.char).mkString(""),
      path = path.map(_.char).mkString(""),
    )
  }
}

case class AsciiMapPath(letters: String, path: String)