package com.artezio.vvishniakou.exercises

object Exercise_2_2 extends App {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length <= 1) {
      true
    } else {
      if (ordered(as(0), as(1)))
        isSorted(as.drop(1), ordered)
      else
        false
    }
  }
  println(isSorted[Int](Array(1, 5, 2, 4, 7, 8, 10), (a, b) => a <= b))
  println(isSorted[Int](Array(1, 2, 4, 5, 7, 8, 10), (a, b) => a <= b))
}
