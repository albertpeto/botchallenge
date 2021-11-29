package botchallenge

import botchallenge.Player.{A, B}
import botchallenge.Rule.Result
import botchallenge.Rule.Result.Continue
import botchallenge.State.Update

object LocalEvaluator {

  def race(state: State, a: Bot[Unit], b: Bot[Unit]): Result = {
    val (updateA, continueA) = runUntilOp(state, A, a)
    val (updateB, continueB) = runUntilOp(state, B, b)

    val nextState = state
      .applyUpdate(A, updateA.getOrElse(Update.DoNothing))
      .applyUpdate(B, updateB.getOrElse(Update.DoNothing))
      .copy(
        previousFields = state.fields,
        round = state.round + 1
      )

    Rule.rules.check(nextState) match {
      case Continue => race(
        nextState,
        continueA.getOrElse(Bot.pure(())),
        continueB.getOrElse(Bot.pure(()))
      )
      case other => other
    }
  }

  private def runUntilOp[T](state: State,
                            player: Player,
                            b: Bot[T]): (Option[Update], Either[T,Bot[T]]) =
    b match {
      case Bot.FlatMap(a, b) => runUntilOp(state, player, b) match {
        case (None, Right(other)) => runUntilOp(state, player, other.asInstanceOf[Bot[T]])
        case (None, Left(t)) => runUntilOp(state, player, a.asInstanceOf[Any => Bot[T]](t))
        case (Some(u), Right(other)) => (Some(u), Right(other.flatMap(a.asInstanceOf[Any => Bot[T]])))
        case (Some(u), Left(t)) => (Some(u), Right(a.asInstanceOf[Any => Bot[T]](t)))
      }
      case Bot.Update(u) => (Some(u), Left(().asInstanceOf[T]))
      case Bot.IsZero() => (None, Left((state.getAt(player) == 0).asInstanceOf[T]))
      case Bot.Pure(a) => (None, Left(a))
    }

}
