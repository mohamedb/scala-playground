package main

import scala.collection.immutable.HashSet

/**
  * Concepts:
  * Functional DS are operated using only pures functions
  * Functional DS are immutable
  */
object FunctionalDataStructure extends App {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ints: List[Double]): Double = ints match {
      case Nil => 1
      case Cons(x, xs) => x * product(xs)
    }

    def tail[A](en: List[A]): List[A] = en match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

    def drop[A](en: List[A], n: Int): List[A] = (en, n) match {
      case (Nil, _) => Nil
      case (a: List[A], 0) => a
      case (a: List[A], c) => drop(tail(a), n - 1)
    }

    def partial1[A, B, C](a: A, f: (A, B) => C): ((B) => C) = {
      return (b: B) => {
        f(a, b)

      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
      l match {
        case Cons(h, t) if f(h) => dropWhile(tail(t), f)
        case _ => l
      }

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def setHead[A](l: List[A], a: A): List[A] = l match {
      case (Nil) => l
      case Cons(x, xs) => Cons(a, xs)
    }

    def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
      case (Nil) => l2 /* Until the first list is exhausted then */
      case Cons(x, xs) => Cons(x, append(xs, l2))
    }

    def init[A](els: List[A]): List[A] = els match {
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }


    val exo = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
  }


  override def main(args: Array[String]): Unit = {
    val u = new HashSet[String]()
    val exampleInts = Cons(1, Cons(2, Cons(10, Nil)))
    val exampleDoubles = Cons(2.0, Cons(0.0, Nil))
    var s = List.sum(exampleInts);
    var p = List.product(exampleDoubles)
    println(s"Sum of [$exampleInts]:  ${s}")
    println(s"Product of [$exampleDoubles]:  ${p}")
    println(s"Variadic  :  ${List.exo}")

    println(s"Tail  :  ${List.tail(List(1, 2, 3))}")
    println(s"Drop  :  ${List.drop(List(1, 2, 3, 4, 5), 2)}")

    println(s"Drop-While  :  ${
      List.dropWhile(List(1, 2, 3, 4, 5), (z: Int) => {
        z == 1
      })
    }")

    println(s"SetHead  :  ${List.setHead(List(1, 2, 3, 4, 5), 7)}")
    println(s"Append  :  ${List.append(List(1, 2, 3, 4, 5), List(6, 7))}")

    println(s"Init  :  ${List.init(List(1, 2, 3, 4, 5))}")

  }


}
