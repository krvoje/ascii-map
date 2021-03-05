package asciipath.model

sealed trait InvalidAsciiMap
case class NoStart()                                 extends InvalidAsciiMap
case class NoEnd()                                   extends InvalidAsciiMap
case class MultipleStarts(cells:          Set[Cell]) extends InvalidAsciiMap
case class MultipleEnds(cells:            Set[Cell]) extends InvalidAsciiMap
case class TForks(cells:                  Set[Cell]) extends InvalidAsciiMap
case class EmptyAsciiMap()                           extends InvalidAsciiMap
case class MultipleRoadsFromStart(cells:  Set[Cell]) extends InvalidAsciiMap
case class MultipleRoadsFromEnd(cells:    Set[Cell]) extends InvalidAsciiMap
case class GapsOnRoad(cells:              Set[Cell]) extends InvalidAsciiMap