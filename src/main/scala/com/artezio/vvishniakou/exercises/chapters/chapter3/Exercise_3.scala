package com.artezio.vvishniakou.exercises.chapters.chapter3

object Exercise_3 extends App {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def tail[A](ls: List[A]): List[A] = ls match {
      case Cons(h, t) => t
      case _ => Nil
    }

    def setHead[A](head: A, list: List[A]): List[A] = Cons(head, list)

    def drop[A](l: List[A], n: Int): List[A] = {
      l match {
        case Nil => Nil
        case Cons(h, t) if n > 0 => drop(t, n - 1)
        case _ => l
      }
    }

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
      l match {
        case Nil => Nil
        case Cons(h, t) if f(h) => dropWhile(t)(f)
        case _ => l
      }
    }

    def init[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(h, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }
    }


    def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      def go(acc: B, l: List[A]): B = {
        l match {
          case Nil => acc
          case Cons(h, t) => go(f(h, acc), t)
        }
      }
      go(z, as)
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)(f)

    def length[A](as: List[A]): Int = foldLeft(as, 0) { (x, y) => 1 + y }

    def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

    def product(as: List[Int]): Int = foldLeft(as, 1)(_ * _)

    def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])(Cons(_, _))

    def append[A](as: List[A], x: A): List[A] = foldRight(as, List(x))(Cons(_, _))

    def concat[A](as: List[A], x: List[A]): List[A] = foldRight(as, x)(Cons(_, _))

    def addOne(as: List[Int]): List[Int] = foldRight(as, Nil: List[Int])((a, b) => Cons(a + 1, b))

    def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((a, b) => concat(f(a), b))

    def filter_2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else Nil)

    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      def go(acc: List[A], l: List[A]): List[A] = {
        l match {
          case Nil => reverse(acc)
          case Cons(h, t) if f(h) => go(Cons(h, acc), t)
          case Cons(h, t) => go(acc, t)
        }
      }
      go(Nil, as)
    }

    def sumLists(l1: List[Int], l2: List[Int]): List[Int] = {
      def go(acc: List[Int], l1: List[Int], l2: List[Int]): List[Int] = {
        l1 match {
          case Nil => l2 match {
            case Nil => reverse(acc)
            case Cons(h, t) => go(Cons(h, acc), l1, t)
          }
          case Cons(h1, t1) => l2 match {
            case Nil => go(Cons(h1, acc), t1, l2)
            case Cons(h, t) => go(Cons(h + h1, acc), t1, t)
          }
        }
      }
      go(Nil, l1, l2)
    }

    def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
      def go(acc: List[A], l1: List[A], l2: List[A]): List[A] = {
        l1 match {
          case Nil => reverse(acc)
          case Cons(h1, t1) => l2 match {
            case Nil => reverse(acc)
            case Cons(h, t) => go(Cons(f(h, h1), acc), t1, t)
          }
        }
      }
      go(Nil, l1, l2)
    }

    def take[A](list: List[A], n: Int): List[A] = {
      def go(acc: List[A], list: List[A], n: Int): List[A] = {
        if (n <= 0) reverse(acc)
        else
          list match {
            case Nil => reverse(acc)
            case Cons(h, t) => go(Cons(h, acc), t, n - 1)
          }
      }
      go(Nil, list, n)
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      sup match {
        case Nil => false
        case Cons(h, t) => sub match {
          case Nil => true
          case Cons(h1, t1) if (h1 == h) && (take(sup, length(sub)) == sub) => true
          case Cons(h1, t1) => hasSubsequence(t, sub)
        }
      }
    }
  }


  println(List.drop(List.setHead(-9, List.tail(List(1, 2, 3, 4, 5))), 3))
  println(List.dropWhile(List.setHead(-9, List.tail(List(1, 2, 3, 4, 5))))(_ < 3))

  println(List.foldRight(List(1, 2, 3), "[")(_ + _ + ",") + "]")

  println(List.foldLeft(List(1, 2, 3), "[")(_ + _ + ",") + "]")

  println(List.length(List(1, 2, 3, 4, 5, 6, 7, 8)))
  println(List.sum(List(1, 2, 3, 4, 5, 6, 7, 8)))
  println(List.product(List(1, 2, 3, 4, 5, 6, 7, 8)))
  println(List.reverse(List(1, 2, 3, 4, 5, 6, 7, 8)))

  println(List.append(List(1, 2, 3, 4, 5, 6, 7, 8), 9))

  println(List.concat(List(1, 2, 3), List(4, 5, 6, 7, 8)))
  println(List.addOne(List.concat(List(1, 2, 3), List(4, 5, 6, 7, 8))))

  println(List.map(List.concat(List(1, 2, 3), List(4, 5, 6, 7, 8)))("'" + _.toString))

  println(List.flatMap(List.concat(List(1, 2, 3), List(4, 5, 6, 7, 8)))(i => List(i, i + 1)))

  println(List.filter(List(1, 9, 3, 4, 5, 6, 7, 8))(_ > 4))

  println(List.filter_2(List(1, 9, 3, 4, 5, 6, 7, 8))(_ > 4))

  println(List.sumLists(List(1, 2, 3), List(4, 5, 6, 7, 8)))

  println(List.zipWith(List(1, 2, 3), List(4, 5, 6, 7, 8))(_ * _))

  println(List.hasSubsequence(List(1, 9, 3, 4, 5, 6, 7, 8), Nil))

}
