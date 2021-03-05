package asciipath

import asciipath.model._

class AsciiMapValidator {
  def validate(asciiMap: AsciiMap): Either[InvalidAsciiMap, AsciiMap] = {
    if (asciiMap.cells.isEmpty) Left {
      EmptyAsciiMap()
    } else if (!asciiMap.cells.exists(_.isStart)) Left {
      NoStart()
    } else if (!asciiMap.cells.exists(_.isEnd)) Left {
      NoEnd()
    } else if (hasTFork(asciiMap)) Left {
      TForks(findTForks(asciiMap))
    } else if (asciiMap.cells.count(_.isStart) > 1) Left {
      MultipleStarts(asciiMap.cells.filter(_.isStart))
    } else if (asciiMap.cells.count(_.isEnd) > 1) Left {
      MultipleEnds(asciiMap.cells.filter(_.isEnd))
    } else if (hasMultipleRoadsFromStart(asciiMap)) Left {
      MultipleRoadsFromStart(findMultipleRoadsFromStart(asciiMap))
    } else if (hasMultipleRoadsFromEnd(asciiMap)) Left {
      MultipleRoadsFromEnd(findMultipleRoadsFromEnd(asciiMap))
    } else if (hasGapsOnRoad(asciiMap)) Left {
      GapsOnRoad(findGapsOnRoad(asciiMap))
    } else Right {
      asciiMap
    }
  }

  private def findTForks(asciiMap: AsciiMap): Set[Cell] = {
    asciiMap.cells.filter { cell =>
      cell.isCrossroads &&
        (asciiMap.left(cell).exists(_.isRoadHorizontal) && asciiMap.right(cell).exists(_.isRoadHorizontal) ||
          (asciiMap.left(cell).exists(_.isCrossroads) && asciiMap.right(cell).exists(_.isCrossroads)) ||
          (asciiMap.up(cell).exists(_.isRoadVertical) && asciiMap.down(cell).exists(_.isRoadVertical)) ||
          (asciiMap.up(cell).exists(_.isCrossroads) && asciiMap.down(cell).exists(_.isCrossroads))
          )
    }
  }

  private def hasTFork(map: AsciiMap): Boolean = findTForks(map).nonEmpty

  // This is a bit going over the top, but it's fun
  private def findMultipleRoadsFrom(asciiMap: AsciiMap, char: Char): Set[Cell] = {
    asciiMap.cells.filter { cell =>
      val hasLeft  = asciiMap.left(cell).nonEmpty
      val hasRight = asciiMap.right(cell).nonEmpty
      val hasUp    = asciiMap.up(cell).nonEmpty
      val hasDown  = asciiMap.down(cell).nonEmpty
      cell.char == char && (
        hasLeft && (hasRight || hasUp || hasDown) ||
          hasRight && (hasLeft || hasUp || hasDown) ||
          hasUp && (hasLeft || hasRight || hasDown) ||
          hasDown && (hasLeft || hasRight || hasUp)
        )
    }
  }

  private def findMultipleRoadsFromStart(map: AsciiMap) = findMultipleRoadsFrom(map, Cell.Start)

  private def hasMultipleRoadsFromStart(map: AsciiMap) = findMultipleRoadsFromStart(map).nonEmpty

  private def findMultipleRoadsFromEnd(map: AsciiMap) = findMultipleRoadsFrom(map, Cell.End)

  private def hasMultipleRoadsFromEnd(map: AsciiMap) = findMultipleRoadsFromEnd(map).nonEmpty

  private def findGapsOnRoad(asciiMap: AsciiMap): Set[Cell] = {
    asciiMap.cells.filter(_.isRoad).filter { road =>
      asciiMap.neighboursExcluding(road).size < 2
    }
  }

  private def hasGapsOnRoad(map: AsciiMap) = findGapsOnRoad(map).nonEmpty
}
