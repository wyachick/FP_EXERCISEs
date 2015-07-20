package com.artezio.vvishniakou.exercises.chapters.chapter4

import scala.util.Try

object Option extends App {

  trait Option[+A] {
    def map[B](f: A => B): Option[B]

    def flatMap[B](f: A => Option[B]): Option[B]

    def getOrElse[B >: A](default: => B): B

    def orElse[B >: A](ob: => Option[B]): Option[B]

    def filter(f: A => Boolean): Option[A]

  }

  case class Some[+A](get: A) extends Option[A] {
    override def map[B](f: (A) => B): Option[B] = Some(f(get))

    override def flatMap[B](f: (A) => Option[B]): Option[B] = f(get)

    override def filter(f: (A) => Boolean): Option[A] = if (f(get)) this else None

    override def getOrElse[B >: A](default: => B): B = get

    override def orElse[B >: A](ob: => Option[B]): Option[B] = this
  }

  case object None extends Option[Nothing] {
    override def map[B](f: (Nothing) => B): Option[B] = None

    override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None

    override def filter(f: (Nothing) => Boolean): Option[Nothing] = None

    override def getOrElse[B >: Nothing](default: => B): B = default

    override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }


  /*
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a.flatMap(x => b.map(y => f(x, y)))
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = {

      def go(acc: List[A], list: List[Option[A]]): Option[List[A]] = {
        list match {
          case Nil => Some(acc.reverse)
          case h :: _ if h == None => None
          case h :: t => val Some(a) = h
            go(a :: acc, t)
        }
      }
      go(Nil, a)
    }
  */

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aa => b map (bb => f(aa, bb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil): Option[List[A]])((x, acc) => map2(x, acc)((x, y) => x :: y))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

    a.foldLeft(Some(Nil): Option[List[B]]) { (b, aa) =>
      f(aa).flatMap(bb => b.map(list => bb :: list))
    }

  }


  def variance(xs: Seq[Double]): Option[Double] = {

    def avg(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty)
        None
      else
        Some(xs.sum / xs.length)
    }

    avg(xs).map(avg => xs.map(x => math.pow(x - avg, 2)).sum / xs.length)
  }

  println(variance(Array(1.0, 3, 2, 4, 54, 6, 32, 34, 4, 3)))
  println(variance(Array[Double]()))

  println(sequence(List(Some(5), Some(54), Some(4))))
  println(sequence(List(Some(5), None, Some(4))))

  println(traverse[String, Int](List("4e4", "123", "12"))(x => Try(x.toInt)))


}