package com.artezio.vvishniakou.exercises.chapters.part1

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

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
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

    def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((a, b) =>
      if (p(a))
        Cons(() => a, () => b)
      else Empty)

    def headOption2: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

    def map[B](f: A => B): Stream[B] = {
      foldRight(Stream[B]())((a, b) => Stream.cons[B](f(a), b))
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      foldRight(Stream[B]())((a, b) => f(a).foldRight(b)((aa, bb) => Stream.cons[B](aa, bb)))
    }

    def append[B >: A](value: => B): Stream[B] = {
      foldRight(Stream(value))((a, b) => Stream.cons(a, b))
    }

    def filter(p: A => Boolean): Stream[A] = {
      foldRight(Stream[A]())((a, b) =>
        if (p(a)) Stream.cons(a, b)
        else b
      )
    }

    def map2[B](f: A => B): Stream[B] = Stream.unfold(this) {
        case Empty => None
        case Cons(h, t) => Some((f(h()), t()))
    }

    def take2(n: Int): Stream[A] = Stream.unfold((this, n)) { x =>
      if (x._2 <= 0) None
      else
        x._1 match {
          case Empty => None
          case Cons(h, t) => Some((h(), (t(), x._2 - 1)))
        }

    }

    def takeWhile3(p: A => Boolean): Stream[A] = Stream.unfold((this, p)) { x =>
      x._1 match {
        case Empty => None
        case Cons(h, t) if x._2(h()) => Some((h(), (t(), x._2)))
        case _ => None
      }

    }

    def zipWith[B](st: Stream[B])(f: (A, B) => B): Stream[B] = Stream.unfold((this, st)) {
        case (Empty, Empty) => None
        case (_, Empty) => None
        case (Empty, _) => None
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold(this, s2) {
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
        case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    }

    def startsWith[B >: A](s: Stream[B]): Boolean = zipAll(s).foldRight(true)((a, b) =>
      a match {
        case (_, None) => true
        case (None, Some(_)) => false
        case _ => (a._1 == a._2) && b
      }
    )

    def tails: Stream[Stream[A]] = Stream.unfold(this) {
        case Empty => None
        case x@Cons(h,t) => Some((x,t()))
    }.append(Empty)

    def hasSubsequence[B >: A](s: Stream[B]): Boolean =
      tails exists (_ startsWith s)

    def scanRight[B](init: B)(f: (A, => B) => B): Stream[B] = Stream.unfold(tails) {
      case Empty => None
      case Cons(h, t) => Some((h().foldRight(init)(f),t()))
    }
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

    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

    def from(n: Int): Stream[Int] = {
      Stream.cons(n, from(n + 1))
    }

    def ones: Stream[Int] = cons(1, ones)

    def fibs: Stream[Int] = {
      def go(prev: Int, prev2: Int): Stream[Int] = {
        Stream.cons(prev + prev2, go(prev2, prev + prev2))
      }
      Stream.cons(0, Stream.cons(1, go(0, 1)))
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case None => Empty
        case Some((a, b)) => Stream.cons(a, unfold(b)(f))
      }
    }


    def constant2[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

    def ones2: Stream[Int] = constant2(1)

    def from2(n: Int): Stream[Int] = unfold(n)(x => Some(n, n + 1))

    def fibs2: Stream[Int] = unfold((0, 1))(x => Some(x._1, (x._2, x._2 + x._1)))


  }


  val y = Stream[Int](1, 2, 5, 6, 7, 8).toList
  println(y)
  println(Stream[Int](1, 2, 5, 6, 7, 8).take(3).toList)

  println(Stream[Int](1, 2, 5, 6, 7, 8).drop(0).toList)

  println(Stream[Int](1, 2, 4, 6, 7, 8).takeWhile(_ < 7).toList)

  println(Stream[Int](1, 2, 4, 6, 7, 8).takeWhile(_ < 5).toList)

  println(Stream[Int](2, 2, 4, 6, 11, 8).forAll(_ % 2 == 0))

  println(Stream[Int](2, 2, 4, 6, 11, 8).headOption2)

  println(Stream[Int](2, 2, 4, 6, 11, 8).map(_ % 2 == 0).toList)

  println(Stream[Int](2, 2, 4, 6, 11, 8).filter(_ % 2 != 0).toList)


  println(Stream[Int](2, 2, 4, 6, 11, 8).flatMap(x => Stream(1 to x: _*)).toList)

  println(Stream[Int](2, 2, 4, 6, 11, 8).flatMap(x => Stream(1 to x: _*)).append({
    56 + 12
  }).toList)


  println(Stream(1, 2).map(_ + 10).filter(_ % 2 == 0).toList)


  println(Stream.ones.take(130).toList)
  println(Stream.ones.takeWhile(_ == 1).take(4).toList)
  println(Stream.ones.forAll(_ != 1))

  println(Stream.constant(2).forAll(_ != 2))

  println(Stream.from(30).takeWhile(_ < 100).toList)

  println(Stream.fibs.take(6).toList)


  println(Stream.fibs2.map2(_ + 12).take(5).toList)
  println(Stream.constant(12).zipWith(Stream.fibs)(_ + _).take(5).toList)
  println(Stream.fibs2.map2(_ + 12).take2(6).toList)
  println(Stream.fibs2.map2(_ + 12).takeWhile3(_ < 11).toList)

  println(Stream.fibs2.zipAll(Stream.from(5)).take(10).toList)

  println(Stream.from(1) startsWith Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

  println(Stream(1,2,3).tails.toList)

  println(Stream().tails.toList)

  println(Stream(1,2,3).scanRight(0)(_ + _).toList)
}
