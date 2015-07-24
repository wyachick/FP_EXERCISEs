package com.artezio.vvishniakou.exercises.chapters.chapter6

object Chapter6 extends App {


  case class State[S, +A](run: S => (A, S)) {


    def map[B](f: A => B): State[S, B] = State(state => {
      val (a, _) = this.run(state)
      (f(a), state)
    })

    def map2[B, C](stateB: State[S, B])(f: (A, B) => C): State[S, C] =
      State[S, C] { state =>
        val (a, _) = this.run(state)
        val (b, _) = stateB.run(state)
        (f(a, b), state)
      }

    def both[B](stateB: State[S, B]): State[S, (A, B)] =
      map2(stateB)((_, _))

    def flatMap[B](g: A => State[S, B]): State[S, B] = State[S, B](state => {
      val (a, _) = run(state)
      g(a).run(state)
    })

    def mapByFlatMap[B](f: A => B): State[S, B] = {
      flatMap(a => State(state => (f(a), state)))
    }

    def map2ByFlatMap[B, C](stateB: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => stateB.flatMap(b => State(state => (f(a, b), state))))
  }


  trait RNG {
    def nextInt: (Int, RNG)
  }

  type Rand[A] = State[RNG, A]

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }


  object State {
    def unit[S, A](a: A) = State[S, A](s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldLeft(State.unit[S, List[A]](List.empty[A]))((b, a) => a.map2(b)(_ :: _))


    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()

  }


  object RNG {

    def int: Rand[Int] = State[RNG, Int](rng => rng.nextInt)

    def nonNegativeInt: Rand[Int] = State[RNG, Int] { rng =>
      val (genVal, newRng) = rng.nextInt
      genVal match {
        case Int.MinValue => (Int.MaxValue, newRng)
        case _ => (math.abs(genVal), newRng)
      }
    }


    def nonNegativeEven: Rand[Int] = nonNegativeInt.map(i => i - i % 2)

    def double: Rand[Double] = int.map(i => i / 1.0)

    def intDouble: Rand[(Int, Double)] = int.both(double)

    def doubleInt: Rand[(Double, Int)] = double.both(int)

    def double3: Rand[(Double, Double, Double)] = double.map2(double.both(double))((b, a) => (a._1, a._2, b))

    def ints(count: Int): Rand[List[Int]] = State(state => {
      def go(acc: List[Int], cnt: Int, rngCurr: RNG): (List[Int], RNG) = {
        if (cnt <= 0) (acc, rngCurr)
        else {
          val (nextInt, rngNext) = rngCurr.nextInt
          go(nextInt :: acc, cnt - 1, rngNext)
        }
      }
      go(Nil, count, state)
    })

    def intsSeq(count: Int): Rand[List[Int]] = State.sequence(List.fill(count)(int))


    /*def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        (mod, rng2)
      else nonNegativeLessThan(n)(rng)
    }*/


    def nonNegativeLessThan(n: Int): Rand[Int] =
      nonNegativeInt.flatMap(i => State(state => {
        val mod = i % n
        if (i + (n - 1) - mod >= 0)
          (mod, state)
        else
          nonNegativeLessThan(n).run(state)
      }))


  }


  val rgn = SimpleRNG(100)
  println(RNG.nonNegativeInt.run(rgn))
  println(rgn.nextInt)
  println(RNG.nonNegativeInt.run(rgn))
  println(RNG.nonNegativeInt.run(rgn))

  val (n, rgn1) = RNG.double.run(rgn)
  println(n)
  println(RNG.double.run(rgn1))

  println(RNG.double3.run(rgn1))
  println(RNG.intDouble.run(rgn1))
  println(RNG.doubleInt.run(rgn1))

  println(RNG.ints(10).run(rgn1))
  println(RNG.ints(10).run(rgn1))


  val int: Rand[Int] = State(s => s.nextInt)

  val intMoreZero: Rand[Int] = RNG.nonNegativeLessThan(12).map(_ + 1)

  println(int.run(SimpleRNG(-100)))


  println(State.sequence(List(RNG.int, RNG.int, RNG.int, RNG.int)).run(rgn))

  println()

  println(RNG.ints(10).run(rgn))
  println(RNG.intsSeq(10).run(rgn))
  println(RNG.intsSeq(10).run(rgn))

  println()
  println(RNG.ints(10).run(rgn1))
  println(RNG.intsSeq(10).run(rgn1))

  println(RNG.nonNegativeLessThan(100).run(rgn))
  println(RNG.nonNegativeLessThan(100).run(rgn))

  val ns: Rand[List[Int]] =
    intMoreZero.flatMap(
      x =>
        intMoreZero.flatMap(y =>
          RNG.ints(x).map(xs =>
            xs.map(_ % y))))

  println(ns.run(rgn)._1, ns.run(rgn)._2)

  val ns1: Rand[List[Int]] = for {
    x <- intMoreZero
    y <- intMoreZero
    xs <- RNG.ints(x)
  } yield xs.map(_ % y)
  println(ns1.run(rgn)._1, ns1.run(rgn)._2)



  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def getInfo: (Int, Int) = (coins, candies)
    def getCandy = copy(locked= true, candies = candies-1, coins = coins + 1)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(machine =>
      inputs.foldLeft((machine.getInfo,machine))((pair, input) =>
        if (pair._2.candies <= 0) pair
        else
          input match {
            case Coin if pair._2.locked => (pair._1, pair._2.copy(locked = false))
            case Coin if !pair._2.locked => pair
            case Turn if !pair._2.locked => val newState = pair._2.getCandy
                                                (newState.getInfo, newState)
            case Turn if pair._2.locked => pair
          }
      )

    )

  println(simulateMachine(List(Turn,Turn,Turn,Turn,Turn,Turn,Turn,Turn,Coin,Coin,Coin,Coin,Coin,Coin,Turn)).run(Machine(locked = true, 5, 10)))

}
