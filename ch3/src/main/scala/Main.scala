object Main extends App {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  // EXERCISE 3.2
  def tail[A](list: List[A]): List[A] =
    list match {
      case Nil           => sys.error("tail of empty list")
      case Cons(_, tail) => tail
    }
}
