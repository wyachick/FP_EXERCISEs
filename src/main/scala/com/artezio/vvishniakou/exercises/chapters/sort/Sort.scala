package com.artezio.vvishniakou.exercises.chapters.sort

object Sort extends App {

  def insertSort[A](list: List[A])(p: (A, A) => Boolean): List[A] = {
    def go(acc: List[A], rest: List[A]): List[A] = {
      println(acc)
      rest match {
        case Nil => acc
        case h :: t => go((acc.takeWhile(p(_, h)) :+ h) ::: acc.dropWhile(p(_, h)), t)
      }
    }
    go(Nil, list)
  }

  def insertSortFold[A](list: List[A])(p: (A, A) => Boolean): List[A] =
    list.foldLeft(List.empty[A])((b, a) =>
      (b.takeWhile(p(_, a)) :+ a) ::: b.dropWhile(p(_, a))
    )





  def mergeSort[A](list: List[A])(p: (A, A) => Boolean): List[A] = {
    def merge(left: List[A], right: List[A]): List[A] = {
      def go(acc: List[A], l: List[A], r: List[A]): List[A] = {
        l match {
          case Nil => r match {
            case Nil => acc.reverse
            case h :: t => go(h :: acc, l, t)
          }
          case hl :: tl => r match {
            case Nil => go(hl :: acc, tl, r)
            case hr :: tr => if (p(hl, hr))
              go(hl :: acc, tl, r)
            else
              go(hr :: acc, l, tr)
          }
        }
      }
      go(Nil, left, right)
    }

    def divide(list: List[A]): (List[A], List[A]) = {
      (list.take(list.size / 2), list.drop(list.size / 2))
    }

    list match {
      case Nil => Nil
      case List(h) => list
      case _ =>
        val (left, right) = divide(list)
        merge(mergeSort(left)(p), mergeSort(right)(p))

    }
  }


  def insertSortArray[A](arr: Array[A])(p: (A, A) => Boolean): Unit = {
    for (j <- 1 until arr.length) {
      val key = arr(j)
      println(arr.toList + " key = " + key)
      var i = j - 1
      while (i >= 0 && !p(arr(i), key)) {
        arr(i + 1) = arr(i)
        i -= 1
      }
      arr(i + 1) = key
    }
  }


  def insertSortArray2[A](arr: Array[A])(p: (A, A) => Boolean): Unit = {
    def go(key: Int): Unit = {
      if (key >= arr.length) ()
      else {
        println(arr.toList + " key = " + arr(key))
        val k = arr(key)
        val ind = arr.indexWhere(!p(_, k))
        if (ind >= 0 && ind < key) {
          for (i <- key until(ind, -1)) {
            val tmp = arr(i)
            arr(i) = arr(i - 1)
            arr(i - 1) = tmp
          }
        }
        go(key + 1)
      }
    }
    go(1)
  }



  println("insertSort")
  println(List(1, 3, 2, 1, 6, 3, 1, 9))
  println(insertSort(List(1, 3, 2, 1, 6, 3, 1, 9))(_ < _))


    val array = Array(1, 3, 2, 1, 6, 3, 1, 9)
  insertSortArray(array)(_ < _)
  println(array.toList)

  val array2 = Array(1, 3, 2, 1, 6, 3, 1, 9)
  insertSortArray2(array2)(_ > _)
  println(array2.toList)

  println("Merge Sort")
  println(mergeSort(List(1, 3, 2, 1, 6, 3, 1, 9, 0))(_ < _))

  println("insertSortFold")
  println(List(1, 3, 2, 1, 6, 3, 1, 9))
  println(insertSortFold(List(1, 3, 2, 1, 6, 3, 1, 9))(_ <= _))

}
