package asciipath.model

sealed trait Movement {
  def opposite: Movement = this match {
    case GoLeft  => GoRight
    case GoRight => GoLeft
    case GoUp    => GoDown
    case GoDown  => GoUp
  }
}
object GoLeft  extends Movement
object GoRight extends Movement
object GoUp    extends Movement
object GoDown  extends Movement
