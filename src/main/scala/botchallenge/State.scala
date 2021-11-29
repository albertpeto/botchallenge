package botchallenge

import botchallenge.State.Update

case class State(player1Pos: Int,
                 player2Pos: Int,
                 fields: Array[Int],
                 previousFields: Array[Int],
                 size: Int,
                 round: Int) {
  def positionFor(player: Player): Int = player match {
    case Player.A => player1Pos
    case Player.B => player2Pos
  }

  def getAt(player: Player): Int =
    fields(positionFor(player))

  def directionFor(player: Player): Int = player match {
    case Player.A => 1
    case Player.B => -1
  }

  def updateAt(player: Player, f: Int => Int): State = {
    val pos = positionFor(player)
    copy(fields = fields.updated(pos, f(fields(pos))))
  }

  def moveForward(player: Player): State = {
    player match {
      case Player.A => copy(player1Pos = player1Pos+1)
      case Player.B => copy(player2Pos = player2Pos-1)
    }
  }

  def moveBackward(player: Player): State = {
    player match {
      case Player.A => copy(player1Pos = player1Pos-1)
      case Player.B => copy(player2Pos = player2Pos+1)
    }
  }

  def applyUpdate(player: Player, update: Update): State = update match {
    case Update.Increment => updateAt(player, _+1)
    case Update.Decrement => updateAt(player, _-1)
    case Update.MoveForward => moveForward(player)
    case Update.MoveBackward => moveBackward(player)
    case Update.DoNothing => this
  }
}

object State {
  sealed trait Update
  object Update {
    case object Increment extends Update
    case object Decrement extends Update
    case object MoveForward extends Update
    case object MoveBackward extends Update
    case object DoNothing extends Update
  }
}