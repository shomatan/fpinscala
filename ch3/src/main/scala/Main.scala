object Main extends App {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  // EXERCISE 3.2
  def tail[A](list: List[A]): List[A] =
    list match {
      case Nil           => sys.error("tail of empty list")
      case Cons(_, tail) => tail
    }

  // EXERCISE 3.3
  def setHead[A](head: A, list: List[A]): List[A] =
    list match {
      case Nil           => sys.error("setHead on empty list")
      case Cons(_, tail) => Cons(head, tail)
    }

  // EXERCISE 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else
      l match {
        case Nil           => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }

    drop(l, n)
  }

  // EXERCISE 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _                           => l
    }

  // EXERCISE 3.6
  // println("init: " + init(Cons(1, Cons(2, Cons(3, Nil)))))
  // init: Cons(1,Cons(2,Nil))
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil              => sys.error("list is empty")
      case Cons(_, Nil)     => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0) { (x, y) => x + y }

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0) { (x, y) => x * y }

  // EXERCISE 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0) { (_, acc) => acc + 1 }

  // EXERCISE 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil              => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

  // EXERCISE 3.11
  def sum(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)
  def product(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  // EXERCISE 3.12
  def reverse[A](list: List[A]): List[A] =
    foldLeft(list, List[A]()) { (acc, item) => Cons(item, acc) }

  // EXERCISE 3.13
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  // EXERCISE 3.14
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  // EXERCISE 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)
}
