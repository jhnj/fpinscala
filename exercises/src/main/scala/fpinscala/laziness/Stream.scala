package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /** 5.1 */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(x, xs) => x() :: xs().toList
  }

  def toListFold: List[A] = foldRight(List[A]())((a, list) => a :: list)

  /** 5.2 */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), Empty)
    case _ => Empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n - 1)
    case Cons(_, t) if n == 1 => t()
    case _ => this
  }

  /** 5.3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  /** 5.4 */
  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)
  /** 5.5 */
  def takeWhileFold(p: A => Boolean): Stream[A] = this.foldRight(empty[A])((a, stream) => {
    if (p(a)) cons(a, stream)
    else Empty
  })

  def headOption: Option[A] = this match {
    case Cons(x, _) => Some(x())
    case _ => None
  }
  /** 5.6 */
  def headOptionFold: Option[A] = this.foldRight(None: Option[A])((a, _) => Some(a))


  /*** 5.7 */
  def mapFold[B](f: A => B): Stream[B] = foldRight(empty[B])((a, stream) => cons(f(a), stream))
  def filterFold(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, stream) => {
    if (p(a)) cons(a, stream)
    else stream
  })
  def append[B >: A](es: => Stream[B]): Stream[B] = foldRight(es)((a, es) => cons(a, es))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, stream) => f(a).append(stream))

  /** 5.13 */
  def mapUnfold[B](f: A => B): Stream[B] = unfold(this)({
    case Cons(x, xs) => Some((f(x()), xs()))
    case Empty => None
  })

  def map[B](f: A => B): Stream[B] = mapUnfold(f)

  def takeUnfold(n: Int): Stream[A] = unfold((n, this))(i => i._2 match {
    case Cons(x, xs) if i._1 > 0 => Some(x(), (i._1 - 1, xs()))
    case _ => None
  })

  def takeWhileUnfold(f: A => Boolean): Stream[A] = unfold(this)({
    case Cons(x, xs) if f(x()) => Some(x(), xs())
    case _ => None
  })

  def zipWithUnfold[B, C](as: Stream[B], f: (A, B) => C): Stream[C] =
    unfold((this, as))({
      case (Cons(x1, xs1), Cons(x2, xs2)) => Some((f(x1(), x2()), (xs1(), xs2())))
      case _ => None
    })

  def zip[B](as: Stream[B]): Stream[(A, B)] =
    zipWithUnfold(as, (a: A, b: B) => (a, b))

  def zipAllUnfold[B, C](as: Stream[B], f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, as))({
      case (Cons(x1, xs1), Cons(x2, xs2)) => Some((f(Some(x1()), Some(x2())), (xs1(), xs2())))
      case (_, Cons(x2, xs2)) => Some((f(None, Some(x2())), (Empty, xs2())))
      case (Cons(x1, xs1), _) => Some((f(Some(x1()), None), (xs1(), Empty)))
      case _ => None
    })
  /** 5.14 */
  def startsWith[B](s: Stream[B]): Boolean =
    zipAllUnfold(s, (ao: Option[A], bo: Option[B]) => (ao, bo))
    .takeWhile(_._2.isDefined).forAll({
      case (h1, h2) => h1 == h2
    })
  /** 5.15 */
  def tails: Stream[Stream[A]] = unfold(this)({
    case Empty => None
    case Cons(x, xs) => Some(cons(x(), xs()), xs())
  }) append Stream(empty)

  /** 5.16 */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, s) => {
      lazy val state = s
      val newB = f(a, state._1)
      (newB, cons(newB, state._2))
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  /** 5.8 */
  def constant[A](c: A): Stream[A] = cons(c, constant(c))
  /** 5.9 */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  /** 5.10 */
  def fibs: Stream[Int] = {
    def go(first: Int, second: Int): Stream[Int] = {
      cons(first, cons(second, go(second, first + second)))
    }

    go(0, 1)
  }
  /** 5.11 */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, s: S)) => cons(a, unfold(s)(f))
  }
  /** 5.12 */
  def fibsUnfold: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def constantUnfold[A](c: A): Stream[A] = unfold(c)(s => Some((s, s)))

  def main(args: Array[String]): Unit = {
    println(fromUnfold(1).take(6).toList)
    println(constantUnfold("string").take(3).toList)
    println(Stream(1,2,3).mapUnfold(_ + 1).takeUnfold(2).toList)
    println(Stream("a", "b", "c").zipAllUnfold(Stream("a", "b"), (ao: Option[String], bo: Option[String]) => {
      for {
        a <- ao
        b <- bo
      } yield a + b
    }).toList)
    println(Stream(1,2,3).startsWith(Stream(1,2)))
    println(Stream(1,2,3).startsWith(Stream(1,2, 3, 4)))
    println(Stream(1,2,3,4).tails.mapUnfold(_.toList).toList)
  }
}