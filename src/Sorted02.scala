object Sorted02 extends App {
  override def main(args: Array[String]): Unit = {
    println(" Sorter?: " + isSortedPolymorphic(
      Array(1, 2, 3),
      (a: Int, b: Int) => {
        println(s" curr: $a <= prev: $b")
        a < b
      }
    ))
    println(" Sort Words by length \n " + isSortedPolymorphic(Array("Fatim", "med", "Salah", "Brahim"),
      (a: String, b: String) => {
        a.length < b.length
      }
    ))


    /**
      * Explicite pour les entiers
      *
      * @param arr
      * @return
      */
    def isSorted(arr: Array[Int]): Boolean = {
      var prev: Int = arr(0)
      for (current <- arr) {
        println(s" curr: $current >= prev: $prev")
        if (current < prev) return false
        prev = current
      }
      true
    }


  }

  /**
    * Utiliser une fonction polymorphic
    *
    * @param arr
    * @param f
    * @tparam A
    * @return
    */
  def isSortedPolymorphic[A](arr: Array[A], f: (A, A) => Boolean): Boolean = {
    def go(i: Int, so: Boolean): Boolean = {
      if (!so) return so
      if (i == arr.length - 1) return true
      val a = arr(i)
      var b = arr(i)
      if (i < arr.length) b = arr(i + 1)
      go(i + 1, f(a, b))
    }

    if (arr.isEmpty) true
    else go(0, true)
  }

  /**
    * Passer par une fonction de comparaison
    *
    * @param arr
    * @param f
    * @return
    */
  def isSortedFn(arr: Array[Int], f: (Int, Int) => Boolean): Boolean = {

    def go(i: Int, so: Boolean): Boolean = {
      if (!so) return so
      if (i == arr.length - 1) return true
      val a = arr(i)
      var b = arr(i)
      if (i < arr.length) b = arr(i + 1)
      go(i + 1, f(a, b))
    }

    if (arr.isEmpty) true
    else go(0, true)
  }
}