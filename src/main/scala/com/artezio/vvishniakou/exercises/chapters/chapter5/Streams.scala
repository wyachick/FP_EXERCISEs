package com.artezio.vvishniakou.exercises.chapters.chapter5

object Streams extends App {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Integer): Stream[A] = {
      if (n <= 0) Empty
      else this match {
        case Empty => this
        case Cons(h, t) => Cons(h, () => t().take(n - 1))
      }
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Empty => this
      case Cons(h, t) => if (p(h()))
        Cons(h, () => t().takeWhile(p))
      else Empty
    }

    def drop(n: Integer): Stream[A] = {
      if (n <= 0) this
      else this match {
        case Cons(h, t) => t().drop(n - 1)
        case _ => Empty
      }
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((a,b) =>
                                                                        if (p(a))
                                                                          Cons(() => a ,() => b)
                                                                        else Empty)



  }


  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }


  val y = Stream[Int](1, 2, 5, 6, 7, 8).toList
  println(y)
  println(Stream[Int](1, 2, 5, 6, 7, 8).take(3).toList)

  println(Stream[Int](1, 2, 5, 6, 7, 8).drop(0).toList)

  println(Stream[Int](1, 2, 4, 6, 7, 8).takeWhile(_ < 7).toList)

  println(Stream[Int](1, 2, 4, 6, 7, 8).takeWhile(_ < 5).toList)

  println(Stream[Int](2, 2, 4, 6, 11, 8).forAll(_  % 2 == 0))





}
