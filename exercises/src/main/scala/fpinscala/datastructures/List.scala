package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def appendRec[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  /** 3.2 */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  /** 3.3 */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  /** 3.4 */
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case list@Cons(_, xs) =>
      if (n > 0) drop(xs, n - 1)
      else list
  }
  /** 3.5 */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case list@Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else list
  }

  /** 3.6 */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

//  def length[A](l: List[A]): Int = {
//    def r(l: List[A], len: Int = 0): Int = l match {
//      case Nil => len
//      case Cons(x, xs) => r(xs, len + 1)
//    }
//
//    r(l)
//  }
  /** 3.9 */
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, z) => z + 1)
  }

  /** 3.10 */
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /** 3.11 */
  def length[A](l: List[A]): Int = {
    foldLeft(l, 0)((z, _) => z + 1)
  }
  def sumFoldLeft(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)
  def productFoldLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  /** 3.12 */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((l, b) => Cons(b, l))
  /** 3.14 */
  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a: A, b: List[A]) => Cons(a, b))

  /** 3.15 */
  def concat[A](as: List[List[A]]): List[A] = foldLeft(as, List[A]())(append)

  /** 3.16 */
  def add1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x+1, add1(xs))
  }
  /** 3.17 */
  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  /** 3.18 */
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x: A, xs) => Cons(f(x), map(xs)(f))
  }

  /** 3.19 */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) Cons(x, filter(xs)(f))
      else filter(xs)(f)
  }

  /** 3.20 */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  /** 3.21 */
  def flatMapFilter[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  /** 3.23 */
  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = l1 match {
    case Nil => Nil
    case Cons(x1, xs1) => l2 match {
      case Nil => Nil
      case Cons(x2, xs2) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
    }
  }
}
