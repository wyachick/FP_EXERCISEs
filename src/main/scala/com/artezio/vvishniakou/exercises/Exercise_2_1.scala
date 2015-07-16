package com.artezio.vvishniakou.exercises

object Exercise_2_1 extends App {

  def fib(n: Int): Int = {

    def go(n: Int, prev: Int, acc: Int): Int = {
      if (n <= 1) {
        acc + prev
      } else {
        go(n - 1, acc, acc + prev)
      }
    }
    n match {
      case x if x <= 0 => 0
      case 1 => 0
      case 2 => 1
      case _ => go(n-2, 0, 1)
    }
  }

  println(fib(-10))
  println(fib(0))
  println(fib(1))
  println(fib(2))
  println(fib(5))
  println(fib(10))

}
