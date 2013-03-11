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

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
  	val (a,rng2) = f(rng)
  	g(a)(rng2)
  }
      
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

//  val positiveInt: Rand[Int] = { rng =>
//    val (thisInt, nextRng) = rng.nextInt
//    if(thisInt != Int.MinValue) (thisInt.abs,nextRng) else positiveInt(nextRng)
//  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a,b)))
  
//  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
//    val (a, rng2) = ra(rng)
//    val (b, rng3) = rb(rng2)
//    (f(a,b),rng3)
//  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldLeft(unit(List[A]()))((acc,f) => map2(f,acc)(_ :: _))
//    
//  def sequenceR[A](fs: List[Rand[A]]): Rand[List[A]] = 
//    fs.foldLeft(unit(List[A]()))((acc,f) => map2(f,acc)(_ :: _))
//    
//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
//    fs match {
//      case Nil => (Nil, rng)
//      case x :: xs => {
//        val (thisX, nextRng) = x(rng)
//        val (rest, finalRng) = sequence(xs)(nextRng)
//        
//        ((thisX :: rest), finalRng)
//      }
//    }
//  }

  
  val positiveInt: Rand[Int] = flatMap(int)( i => if(i != Int.MaxValue) unit(i) else int )  
    
  val double:Rand[Double] = map(positiveInt)( i => i/(Int.MaxValue.toDouble+1) )

  val intDouble:Rand[(Int,Double)] = map2(int,double)((i,d) => (i,d))

  val doubleInt:Rand[(Double,Int)] = map2(double,int)((d,i) => (d,i))

  def double3:Rand[(Double,Double,Double)] = { rng =>
    val (d1,rng2) = double(rng)
    val (d2,rng3) = double(rng2)
    val (d3,rng4) = double(rng3)
    
    ((d1,d2,d3), rng4)
  }

  def ints(count: Int):Rand[List[Int]] = sequence(List.fill(count)(int))

  def positiveMax(n: Int): Rand[Int] = map(double)( d => (n * d).toInt )


}

import State._

case class State[S,+A](run: S => (A, S)) {
  
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State( s => {
    val (a,s1) = run(s)
    f(a).run(s1)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def unit[S,A](a: A): State[S,A] = State( s => (a,s) )  
  def simulateMachine(inputs: List[Input]): State[Machine, Int] = sys.error("todo")
}
