package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We will later define other functions in terms of `nextInt`.
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & // `&` is bitwise AND
                  ((1L << 48) - 1) // `<<` is left binary shift
      ((seed2 >>> 16).asInstanceOf[Int], // `>>>` is right binary shift with zero fill
       simple(seed2))
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (thisInt, nextRng) = rng.nextInt
    if(thisInt != Int.MinValue) (thisInt.abs,nextRng) else positiveInt(nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (thisInt, nextRng) = rng.nextInt
    val d = thisInt.toDouble / Int.MaxValue.toDouble
    (d,nextRng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,rng2) = rng.nextInt
    val (d,rng3) = double(rng2)
    ((i,d),rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d,rng2) = double(rng)
    val (i,rng3) = rng2.nextInt
    ((d,i),rng3)
  } 

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,rng2) = double(rng)
    val (d2,rng3) = double(rng2)
    val (d3,rng4) = double(rng3)
    
    ((d1,d2,d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (Nil, rng)
    else {
      val (thisInt, nextRng) = rng.nextInt
      val (rest, finalRng) = ints(count-1)(nextRng)
      
      (thisInt :: rest, finalRng)
    }
  }

  def positiveMax(n: Int): Rand[Int] = sys.error("todo")

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = sys.error("todo")

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = sys.error("todo")

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = sys.error("todo")
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, Int] = sys.error("todo")
}

object test {
  def main(args: Array[String]) = {
    
    val res = RNG.ints(5)(RNG.simple(5L))
    
    println(res)
    
  }
}