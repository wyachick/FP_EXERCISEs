package com.artezio.vvishniakou.exercises

import Exercise_2_3._

object Exercise_2_4 extends App {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  println(uncurry(curry((a: Int, b: Int) => a < b))(2, 5))

}
