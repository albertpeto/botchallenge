package botchallenge

import botchallenge.Player.{A, B}
import botchallenge.Rule.Result
import botchallenge.Rule.Result.{Continue, Draw, Winner}

case class Rule(check: State => Result) {
  def and(other: Rule): Rule = Rule { s =>
    check(s).combine(other.check(s))
  }
}

object Rule {

  sealed trait Result {
    def combine(other: Result): Result = (this,other) match {
      case (Draw, _)              => Draw
      case (_, Draw)              => Draw
      case (Winner(A), Winner(B)) => Draw
      case (Winner(B), Winner(A)) => Draw
      case (Continue,other)       => other
      case (other,Continue)       => other
    }
  }
  object Result {
    case object Draw extends Result
    case class Winner(player: Player) extends Result
    case object Continue extends Result
  }

  def when(cond: State => Boolean)(r: Result): Rule = Rule {
    s => if (cond(s)) r else Continue
  }
  def aPositionTooSmall: Rule = when(_.player1Pos < 0)(Winner(B))
  def bPositionTooSmall: Rule = when(_.player2Pos < 0)(Winner(A))
  def aPositionTooLarge: Rule = when(s => s.player1Pos >= s.fields.length)(Winner(B))
  def bPositionTooLarge: Rule = when(s => s.player2Pos >= s.fields.length)(Winner(B))
  def aFlagZeroedOut: Rule    = when(s => s.fields(0) == 0 && s.previousFields(0) == 0)(Winner(B))
  def bFlagZeroedOut: Rule    = when(s => s.fields(s.fields.length-1) == 0 && s.previousFields(s.fields.length-1) == 0)(Winner(A))
  def tooManyRounds: Rule     = when(_.round == 100)(Draw)

  def rules = aPositionTooSmall and
    bPositionTooSmall and
    aPositionTooLarge and
    bPositionTooLarge and
    aFlagZeroedOut and
    bFlagZeroedOut and
    tooManyRounds
}
