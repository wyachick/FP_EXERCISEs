package com.artezio.vvishniakou.exercises.chapters.chapter6

object Chapter6 extends App {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object RNG {
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (genVal, newRng) = rng.nextInt
      genVal match {
        case Int.MinValue => (Int.MaxValue, newRng)
        case _ => (math.abs(genVal), newRng)
      }
    }

    def double(rgn: RNG): (Double, RNG) = {
      val (genVal, newRng) = nonNegativeInt(rgn)
      genVal match {
        case Int.MaxValue => ((genVal.toDouble - 1) / Int.MaxValue, newRng)
        case 0 => (0, newRng)
        case _ => (genVal.toDouble / Int.MaxValue, newRng)
      }
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (n, rgn1) = rgn.nextInt
      val (f, rgn2) = double(rgn1)
      ((n, f), rgn2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((n, f), rgn1) = intDouble(rgn)
      ((f, n), rgn1)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rgn1) = double(rgn)
      val (d2, rgn2) = double(rgn1)
      val (d3, rgn3) = double(rgn2)
      ((d1, d2, d3), rgn3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      def go(acc: List[Int], cnt: Int, rngCurr: RNG): (List[Int], RNG) = {
        if (cnt <= 0) (acc, rngCurr)
        else {
          val (nextInt, rngNext) = rngCurr.nextInt
          go(nextInt :: acc, cnt - 1, rngNext)
        }
      }
      go(Nil, count, rng)
    }
  }

  val rgn = SimpleRNG(-100)
  println(RNG.nonNegativeInt(rgn))
  println(rgn.nextInt)
  println(RNG.nonNegativeInt(rgn))
  println(RNG.nonNegativeInt(rgn))

  val (n, rgn1) = RNG.double(rgn)
  println(n)
  println(RNG.double(rgn1))

  println(RNG.double3(rgn1))
  println(RNG.intDouble(rgn1))
  println(RNG.doubleInt(rgn1))

  println(RNG.ints(10)(rgn1))
  println(RNG.ints(10)(rgn1))
  println(RNG.ints(10)(rgn))

}
