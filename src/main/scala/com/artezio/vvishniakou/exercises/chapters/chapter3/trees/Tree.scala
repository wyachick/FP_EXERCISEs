package com.artezio.vvishniakou.exercises.chapters.chapter3.trees

object Tree extends App {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  object Tree {

    def size[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
      }
   }

    def maximum(tree: Tree[Int]): Int = {
      tree match {
        case Leaf(x) => x
        case Branch(l, r) => maximum(l) max maximum(r)
      }
    }

    def depth[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(x) => 1
        case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
      }
    }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
        tree match {
          case Leaf(x) => Leaf(f(x))
          case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        }
    }

    def fold[A, B](tree: Tree[A], init: B)(f: (A, B) => B): B = {
      tree match {
        case Leaf(x) => f(x, init)
        case Branch(l, r) =>  fold(r, fold(l, init)(f))(f)
      }
    }
  }

  println(Tree.size(Branch(Branch(Leaf(5),Leaf(4)),Leaf(1))))

  println(Tree.maximum(Branch(Branch(Leaf(15),Leaf(34)),Leaf(1))))

  println(Tree.depth(Branch(Branch(Leaf(15),Leaf(34)),Leaf(1))))

  println(Tree.map(Branch(Branch(Leaf(15),Leaf(34)),Leaf(1)))(_ * -1))

  println(Tree.fold(Branch(Branch(Leaf(15),Leaf(34)),Leaf(1)), 12)(_ + _))

}
