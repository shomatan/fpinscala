import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    println("EXERCISE 2.1")
    println("fib(10): " + fib(10))

    println("")

    println("EXERCISE 2.2")
    val arr1 = Array(1, 2, 3, 4, 5)
    println("isSorted: " + isSorted(arr1, { (n1: Int, n2: Int) => n1 > n2 }))
    println("isSorted: " + isSorted(arr1, { (n1: Int, n2: Int) => n1 < n2 }))
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, current: Int): Int =
      if (n == 0) prev
      else loop(n - 1, current, prev + current)

    loop(n, 0, 1)
  }

  // Implement a polymorphic function to check whether
  // an `Array[A]` is sorted
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1) true // Finish: length is equal to n
      else if (!ordered(as(n), as(n + 1))) false
      else go(n + 1)

    go(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncarry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
