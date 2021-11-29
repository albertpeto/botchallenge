package botchallenge

import Bot.{isZero, _}

/*
 * Based on https://codegolf.stackexchange.com/questions/36645/brainfedbotsforbattling-a-brainf-tournament
 *
 * The rules are somewhat different, for example checking if a field is zero is not
 * considered an instruction cycle.
 *
 * Also fields don't wrap because this was just an experiment.
 */
object Main {

  def main(args: Array[String]): Unit = {
    val bot1 = for {
      _ <- times(9)(moveForward)
      _ <- repeatWhile(not(isZero))(decrement)
    } yield ()

    val bot2 = moveForward

    val start = Array(10, 0, 0, 0, 0, 0, 0, 0, 0, 10)

    val result = botchallenge.LocalEvaluator.race(
      State(0, 9, start, start, 10, 0),
      bot1,
      bot2
    )

    println(result)
  }

}
