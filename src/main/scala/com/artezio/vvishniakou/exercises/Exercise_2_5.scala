package com.artezio.vvishniakou.exercises

object Exercise_2_5 extends App {
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  println(compose[Int, Int, String]("_"*_, _ * 5)(5))

}
