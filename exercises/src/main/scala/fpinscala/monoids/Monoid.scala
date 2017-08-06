package fpinscala.monoids

import java.util.concurrent.ForkJoinPool

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps
import fpinscala.testing.Prop

import language.higherKinds
import scala.util.Random

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 match {
      case s: Some[A] => s
      case None => a2
    }
    def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    def op(a1: (A) => A, a2: (A) => A): (A) => A = a1.compose(a2)
    def zero: (A) => A = a => a
  }

  // reverse the arguments of the monoid
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)
    def zero: A = m.zero
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
//  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val associative =
      forAll(for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z)) { case (x, y, z) => {
        m.op(m.op(x, y), z) == m.op(x, m.op(y, z))
      }}

    val zero =
      forAll(gen)(x => {
        m.op(x, m.zero) == x && m.op(m.zero, x) == x
      })

    associative && zero
  }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])((a: A) => {
      (b: B) => f(a, b)
    })(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
   def inner(start: Int, end: Int): B = {
     if (start - end == 0) {
       f(as(start))
     } else if (start - end == 1) {
       m.op(f(as(start)), f(as(end)))
     } else {
       m.op(inner(start, start + (end - start) / 2), inner(start + (end - start) / 2 + 1, end))
     }
   }
   if (as.isEmpty) m.zero
   else inner(0, as.length - 1)
  }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val parM = par(m)
    def inner(start: Int, end: Int): Par[B] = {
      if (start - end == 0) {
        Par.unit(f(v(start)))
      } else if (start - end == 1) {
        Par.unit(m.op(f(v(start)), f(v(end))))
      } else {
        parM.op(inner(start, start + (end - start) / 2), inner(start + (end - start) / 2 + 1, end))
      }
    }
    if (v.isEmpty) parM.zero
    else inner(0, v.length - 1)
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c1), Stub(c2)) =>
        Stub(c1 + c2)
      case (Stub(c1), Part(st, n, end)) =>
        Part(c1 + st, n, end)
      case (Part(st, n, end), Stub(c2)) =>
        Part(st, n, end + c2)
      case (Part(st1, n1, end1), Part(st2, n2, end2)) =>
        val newWords =
          if ((end1 + st2).length > 0) 1
          else 0
        Part(st1, n1 + newWords + n2, end2)
    }
    val zero: WC = Stub("")
  }

  def count(s: String): Int = {
    def toWC(c: Char) = {
      if (c.isWhitespace)
        Part("", 0, "")
      else Stub(c.toString)
    }
    def isWord(s: String) = if (s.length > 0) 1 else 0
    foldMapV(s.toIndexedSeq, wcMonoid)(toWC) match {
      case Stub(_) => 0
      case Part(st, n, end) => isWord(st) + n + isWord(end)
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    def zero: (A, B) = (A.zero, B.zero)
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1(a), a2(a))
    def zero: (A) => B = _ => B.zero
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K,V]] {
      def zero: Map[K, V] = Map[K,V]()
      def op(a1: Map[K,V], a2: Map[K,V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(zero)((m, k) => {
          m.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
        })
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    IndexedSeqFoldable.foldMap(as)(k => Map(k -> 1))(mapMergeMonoid(intAddition))
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    def reversedCurried(a: A) = (b: B) => f(b, a)
    foldMap(as)(reversedCurried)(dual(endoMonoid[B]))(z)
  }
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())((a, list) => a :: list)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b,a) => mb.op(b, f(a)))
  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(a) => f(z, a)
    case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(a) => f(a, z)
    case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }
//    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(a) => f(z, a)
  }
//    as.foldLeft(z)(f)
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a) => f(a, z)
  }
//    as.foldRight(z)(f)
}

object TestMonoidLaws {
  import Monoid._
  import fpinscala.testing.Gen._

  def main(args: Array[String]) = {
    val stringLaw = monoidLaws(stringMonoid, choose(0, 10).flatMap(len => string(len)))
    Prop.run(stringLaw)

    val addLaw = monoidLaws(intAddition, choose(-100, 100))
    Prop.run(addLaw)

    val multiplicationLaw = monoidLaws(intMultiplication, choose(-100, 100))
    Prop.run(multiplicationLaw)

    val rand = new Random(1)
    val numbers = IndexedSeq.fill(1000)(rand.nextInt).map(_.toString)//"1","3","4","5","2")
    val sum = numbers.map(_.toInt).sum
    assert(foldMapV(numbers, intAddition)(_.toInt) == sum)

    val parfold = parFoldMap(numbers, intAddition)(_.toInt)
    assert(Par.run(new ForkJoinPool())(parfold) == sum)

    assert(count("first second third f") == 4)

    assert(bag(IndexedSeq("a", "a", "b")) == Map("a" -> 2, "b" -> 1))
  }
}

