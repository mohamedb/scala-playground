package main

object Utils {

  def formatResult(desc: String, n: Int, f: Int => Int): String = {
    val msg = " La: %s de %d est %d"
    msg.format(desc, n, f(n))
  }
}
