object MainApp extends App {
  override def main(args: Array[String]): Unit = {
    println(s"Fact m1 : " + factorial(7));
    println(s"Fact m2 :  " + factAccum(7));
    println(s"Fib 4:  " + fib(4));
    println(s"Fib 6:  " + fib(6));
    println(s"Fib 7:  " + fib(7));
  }

  def factorial(to: Int): Int = {
    def loop(t: Int): Int = {
      if (t <= 1) 1
      else t * factorial(to - 1)
    }

    loop(to)
  }

  def factAccum(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 1) acc;
      else go(n - 1, acc * n);
    }

    go(n, 1);
  }

  def fib(n: Int): Int = {

    def go(t: Int, s: Int): Int = {
      1
    }

    go(n, 0)
  }
}
