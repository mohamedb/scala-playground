/**
  * Objective: get familiar with loops without using For
  * Should be a 'tail-recursive algorithm'
  */
object Fib01 extends App {
  override def main(args: Array[String]): Unit = {
    println(s"Fib 0:  " + fibTwo(0) + " should be: 0")
    println(s"Fib 1:  " + fibTwo(1) + " should be: 1")
    println(s"Fib 2:  " + fibTwo(2) + " should be: 1")
    println(s"Fib 3:  " + fibTwo(3) + " should be: 2")
    println(s"Fib 4:  " + fibTwo(4) + " should be: 3")
    println(s"Fib 6:  " + fibTwo(6) + " should be: 8")
    println(s"Fib 7:  " + fibTwo(7) + " should be: 13")
    println(s"Fib 8:  " + fibTwo(8) + " should be: 21")
    println(s"Fib 9:  " + fibTwo(9) + " should be: 34")
    println(Utils.formatResult("FibTwo", 10, fibTwo))

    println(Utils.formatResult("fib ", 10, fib))
  }

  /**
    * Implémentation clean
    * Impl semblable a un While
    *
    * @param n entree
    * @return
    */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(v: Int, p1: Int, p0: Int): Int = {
      if (v == 0) p0
      else if (v == 1) p1
      else go(v - 1, p1 + p0, p1)
    }

    go(n, 1, 0)
  }

  /**
    * L'indée est de passé les paramètres necessaires comme arguements
    * implémentation qui ressemble a un For avec incrémentation
    *
    * @param n entree
    * @return
    */
  def fibTwo(n: Int): Int = {
    @annotation.tailrec
    def go(lim: Int, v: Int, prev_1: Int, prev_2: Int): Int = {
      if (v == lim - 2) prev_1 + prev_2
      else go(lim, v + 1, prev_1 + prev_2, prev_1)
    }

    if (n == 0) 0
    else if (n == 1) 1
    else go(n, 0, 1, 0)

  }


  /**
    * Factoriel en utilisant la methode non optimisée
    *
    * @param to
    * @return
    */
  def factorial(to: Int): Int = {
    def loop(t: Int): Int = {
      if (t <= 1) 1
      else t * factorial(to - 1)
    }

    loop(to)
  }

  /**
    * Version optimisée de recursion en passant par variable qui accumule le resultat de chaque recursion
    *
    * @param n
    * @return
    */
  def factAccum(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 1) acc
      else go(n - 1, acc * n)
    }

    go(n, 1)
  }

}
