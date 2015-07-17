package com.artezio.vvishniakou.exercises.chapters.chapter2

object Exercise_2_3 extends App {
  def curry[A,B,C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  println(curry((a: Int, b: Int) => a > b)(5)(6))

}
