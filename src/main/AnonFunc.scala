package main

object AnonFunc extends App {

  override def main(args: Array[String]): Unit = {
    val fn = (other: String) => {
      s"Aussi salutations à : $other"
    }
    println(salutation("Med", "Salah", fn))
 
  }

  def salutation(from: String, to: String, x: String => String): String = {
    s" ${from.toUpperCase}: Bonjour ${to.toUpperCase()} ${x("notre frère Brahim")}"
  }
}
