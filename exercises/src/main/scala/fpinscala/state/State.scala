package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (res, r) = rng.nextInt
    res match {
      case Int.MinValue => (Int.MaxValue, r)
      case _            => (if (res < 0) -res else res, r)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1.0), r)
  }

  def doubleViaMap(rng: RNG): Rand[Double] = {
    map(int)(i => i.toDouble / (Int.MaxValue.toDouble + 1.0))
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (List[Int](), rng)
    } else {
      val (result, r1) = ints(count-1)(rng)
      val (i, r2) = r1.nextInt
      (i :: result, r2)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = (rng: RNG) => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(unit(List[A]()))((x, z) => map2(x, z)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, r1) = f(rng)
    val randB = g(a)
    randB(r1)
  }

  def mapViaFlatMap[A,B](r: Rand[A])(f: A => B): Rand[B] = 
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = this.flatMap[B](a => State.unit(f(a)))
    
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = this.flatMap(a => sb.map(f(a, _)))
    
  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B]({ s1 =>
    val (a, s2) = this.run(s1)
    f(a).run(s2)
  })
    
}

sealed trait Input {
  def apply(): State[Machine, (Int, Int)]
}

case object Coin extends Input {
  def apply(): State[Machine, (Int, Int)] = State({ m =>
    if (m.locked && m.candies > 0) ((m.coins+1, m.candies), m.copy(locked = false, coins = m.coins+1))
    else ((m.coins, m.candies), m)
  })
}
case object Turn extends Input {
  def apply(): State[Machine, (Int, Int)] = State({ m =>
    if (!m.locked) ((m.coins, m.candies-1), m.copy(locked = true, candies = m.candies-1))
    else ((m.coins, m.candies), m)
  })
}

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State({ m =>
    var mResult = inputs.foldLeft(m)((acc, input) => input().run(acc)._2)
    ((mResult.coins, mResult.candies), mResult)
  })
  def unit[S,A](a: A) = State((s: S) => (a, s))
  def sequence[S,A](fs: List[State[S,A]]): State[S, List[A]] = 
    fs.foldRight[State[S,List[A]]](unit(List[A]()))((x, z) => x.map2(z)(_ :: _))
}
