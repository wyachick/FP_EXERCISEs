package com.artezio.vvishniakou.exercises.chapters.chapter4

object Eithers extends App {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }

  case class Left[+E](value: E) extends Either[E, Nothing] {

    override def map[B](f: (Nothing) => B): Either[E, B] = this

    override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C) = this

    override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] = this

    override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
  }


  case class Right[+A](value: A) extends Either[Nothing, A] {

    override def map[B](f: (A) => B): Either[Nothing, B] = Right(f(value))

    override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C) = b.map(bb => f(value, bb))

    override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] = f(value)

    override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this
  }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldLeft(Right(Nil): Either[E, List[A]])(_.map2(_)((t, h) => h :: t))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldLeft(Right(Nil): Either[E, List[B]])((b, a) => b.map2(f(a))((t, h) => h :: t))
  }


  println(sequence(List(Right(20), Left("Error1"), Right(30), Left("Error2"))))

  println(traverse(List("20", "Error1", "30", "Error2"))(x => Try(x.toInt)))

  case class Person(name: Name, age: Age)

  sealed class Name(val value: String)

  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[List[String], Person] =
    mkName(name) match {
      case Left(eName) => mkAge(age) match {
        case Left(eAge) => Left(List(eName, eAge))
        case Right(_) => Left(List(eName))
      }
      case Right(n) => mkAge(age) match {
        case Left(eAge) => Left(List(eAge))
        case Right(a) => Right(Person(n, a))
      }
    }

  println(mkPerson("", -19))


}
