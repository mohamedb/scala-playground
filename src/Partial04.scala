object Partial04 extends App {
  override def main(args: Array[String]): Unit = {
    /** Un petit exemple pour tester */
    val compareFn = (a: Int, b: Int) => {
      a == b
    }
    println("Equality checker: 1 == 3 " + partial1(1, compareFn)(3))

    /* reapliquer sur les exemples d'avant */
    println("Array sorted? " + partial1(Array(1, 2, 7, 5, 8, 11), Sorted02.isSortedFn)((a: Int, b: Int) => {
      println(s" curr: $a <= prev: $b")
      a < b
    }))

  }

  /**
    * a higher-order function for doing what is called
    * partial application. This function, , takes a value and a function of two partial1
    * arguments, and returns a function of one argument as its result
    */
  def partial1[A, B, C](a: A, f: (A, B) => C): ((B) => C) = {
    return (b: B) => {
      f(a, b)

    }
  }
}
