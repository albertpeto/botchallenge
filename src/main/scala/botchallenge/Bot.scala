package botchallenge

import Bot._

sealed trait Bot[A] {
  def map[B](f: A => B): Bot[B] = flatMap(a => pure(f(a)))
  def flatMap[B](f: A => Bot[B]): Bot[B] = FlatMap(f, this)
}

object Bot {

  def pure[A](a: A): Bot[A] = Pure(a)

  def repeatWhile[T](b: Bot[Boolean])(c: Bot[T]): Bot[List[T]] =
    b.flatMap {
      case true => for {
        t <- c
        ts <- repeatWhile(b)(c)
      } yield t :: ts
      case false => pure(List.empty)
    }

  def forever[T](b: Bot[T]): Bot[List[T]] =
    repeatWhile(pure(true))(b)

  def times[T](x: Int)(b: Bot[T]): Bot[List[T]] =
    if (x > 0) {
      for {
        t <- b
        ts <- times(x-1)(b)
      } yield t :: ts
    } else {
      pure(List.empty)
    }

  def not(b: Bot[Boolean]): Bot[Boolean] = b.map(!_)

  case class FlatMap[A,B](a: A => Bot[B], b: Bot[A]) extends Bot[B]
  case class Update(update: State.Update) extends Bot[Unit]
  case class IsZero() extends Bot[Boolean]
  case class Pure[A](a: A) extends Bot[A]

  val moveForward: Bot[Unit] = Update(State.Update.MoveForward)
  val moveBackward: Bot[Unit] = Update(State.Update.MoveBackward)
  val increment: Bot[Unit] = Update(State.Update.Increment)
  val decrement: Bot[Unit] = Update(State.Update.Decrement)
  val isZero: Bot[Boolean] = IsZero()
}
