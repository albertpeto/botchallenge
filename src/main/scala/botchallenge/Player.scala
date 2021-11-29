package botchallenge

sealed trait Player
object Player {
  case object A extends Player
  case object B extends Player
}