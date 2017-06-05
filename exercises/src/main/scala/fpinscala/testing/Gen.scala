package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}


/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

/** 8.1
  *   sum: List[Int] => Int
  *   sum(list) == sum(list.reverse)
  *   sum(list.fill(n)(integer)) == n * integer
  *   sum(list) == sum(list.take(n)) + sum(list.drop(n))
  *   sum(-sum(list) :: list) == 0
  */

/** 8.2
  *   max: List[Int] => Int
  *   max(list) == max(list.reverse)
  *   max(list.fill(n)(integer)) == integer
  *   sum(list) == sum(list.take(n)) max sum(list.drop(n))
  */

/** 8.3
  */
trait PropV1 {
  def check: Boolean
  def &&(that: PropV1) = new PropV1 { def check: Boolean = this.check && that.check }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def apply(run: (TestCases, RNG) => Result): Prop = {
    Prop { (_, t, rng) => run(t, rng) }
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(i => g.forSize(i))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n,rng) => {
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) => {
          p.run(max, casesPerSize, rng)
        }}).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }
  }

  val S: Gen[ExecutorService] = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))){ case es ** a => f(a)(es).get}


  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop {
    (_, _, _) => {
      if (p) Proved else Falsified("()", 0)
    }
  }

  def checkPar(p: Par[Boolean]): Prop = forAllPar(unit(()))((_) => p)

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)


}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  /** 8.9
    *
    */
  def &&(that: Prop) = Prop {
    (maxSize, n, rng) => run(maxSize, n, rng) match {
      case Passed | Proved => that.run(maxSize, n, rng)
      case Falsified(msg, s) => Falsified(msg, s)
    }
  }

  def ||(that: Prop) = Prop {
    (maxSize, n, rng) => run(maxSize, n, rng) match {
      case Falsified(_, _) => that.run(maxSize, n, rng)
      case p => p
    }
  }

//  def ||(that: Prop) = Prop {
//    (n, rng) => run(n, rng) match {
//      case Falsified => that.run(n, rng)
//      case Passed => that.run(n, rng)
//    }
//  }
}

object Gen {
  /** 8.4
    *
    */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start)))
  }
//  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
//    Gen(State(RNG.double).map(i => start + (i * (stopExclusive - start)).toInt))
//  }

  /** 8.5
    *
    */
  def unit[A](a: => A): Gen[A] = {
    Gen(State(s => (a,s)))
  }
  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }
  def pair[A](g: Gen[A]): Gen[(A, A)] = {
    Gen(g.sample.map2(g.sample)((_, _)))
  }
  def option[A](g: Gen[A]): Gen[Option[A]] = {
    Gen(State(RNG.boolean).map2(g.sample)((b, s) => if (b) Some(s) else None))
  }

  /** 8.7
    *
    */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    Gen(State(RNG.boolean).flatMap { if (_) g1.sample else g2.sample })
  }

  /** 8.8
    *
    */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    Gen(State(RNG.double).flatMap { d => if (d < g1._2 / (g1._2 + g2._2)) g1._1.sample else g2._1.sample })
  }

  /** 8.12
    *
    */
  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(len => listOfN(len, g))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(len => listOfN(len max 1, g))
  }

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

  /** 8.16
    *  Par[Int] generator
    */
  val parGen: Gen[Par[Int]] = {
    val gen = choose(-10, 10).listOfN(choose(0, 10))
    gen.map(list => {
      list.foldLeft(Par.unit(1))((v: Par[Int], i: Int) => {
        Par.fork(Par.map2(v, Par.unit(i))(_ + _))
      })
    })
  }
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))
  /** 8.6
    *
    */
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(l => Gen(State.sequence(List.fill(l)(sample))))
  }

  /** 8.10
    *
    */
  def unsized: SGen[A] = {
    SGen(_ => this)
  }

  def **[B](that: Gen[B]): Gen[(A, B)] = {
    this.map2(that)((_, _))
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(i: Int): Gen[A] = forSize(i)
}

object testRun {
  def main(args: Array[String]): Unit = {
    // test max
    val smallInt = Gen.choose(-10,10)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)
    /** 8.14
      * test List.sorted
      */
    val sortedProp = forAll(listOf1(smallInt)) { ns =>
      val sorted = ns.sorted
      val sorted2 = ns.reverse.sorted
      sorted == sorted2
    } && forAll(listOf1(smallInt)) { ns =>
      val sorted = ns.sorted
      sorted.head == sorted.min
    }
    Prop.run(sortedProp)

    val p2 = checkPar(
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    )
    Prop.run(p2)


    val pint = Gen.choose(0,10) map Par.unit
    val p4 =
      forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

    Prop.run(p4)

    /** 8.17
      * fork(x) == x
      */
    val pFork = forAllPar(pint)(n => equal(Par.fork(n), n))

    /** 8.18
      *   takeWhile
      */
    val takeWhileProp =
      forAll(listOf(smallInt)) { ns =>
        ns.takeWhile(_ > 0).forall(_ > 0)
      } && forAll(listOf(smallInt)) { ns =>
        def fun(i: Int) = i > 0
        ns.takeWhile(fun).length == ns.length - ns.dropWhile(fun).length
      }

  }
}