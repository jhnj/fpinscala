package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._
import fpinscala.applicative.Applicative.Identity
import fpinscala.testing.{Gen, Prop}
import monoids._

import language.higherKinds
import language.implicitConversions
import scala.runtime.Nothing$

trait Applicative[F[_]] extends Functor[F] {

  /** 12.2
    *
    * Hard: The name applicative comes from the fact that we can formulate the Applicative
    * interface using an alternate set of primitives, unit and the function apply , rather than
    * unit and map2. Show that this formulation is equivalent in expressiveness by defining
    * map2 and map in terms of unit and apply . Also establish that apply can be imple-
    * mented in terms of map2 and unit.
    */
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    // Intellij complains about this
    // apply(map(fa)(f.curried))(fb)
    val fab = map(fa)(f.curried)
    apply(fab)(fb)
  }
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    val fab: F[(A) => B] = unit(f)
    apply(fab)(fa)
  }

  /** 12.3
    * The apply method is useful for implementing map3, map4, and so on, and the pattern
    * is straightforward. Implement map3 and map4 using only unit, apply , and the curried
    * method available on functions.
    */
  def map3[A,B,C,D](fa: F[A],
                    fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] =
    map2(map2(fa, fb)(f.curried(_)(_)), fc)((fCurried, c) => fCurried(c))

  def map4[A,B,C,D,E](fa: F[A],
                      fb: F[B],
                      fc: F[C],
                      fd: F[D])(f: (A, B, C, D) => E): F[E] =
    map2(map3(fa, fb, fc)(f.curried(_)(_)(_)), fd)((fCurried, d) => fCurried(d))

  /** 12.1
    *
    * Transplant the implementations of as many combinators as you can from Monad to
    * Applicative, using only map2 and unit, or methods implemented in terms of them.
    */
  def sequence[A](fas: List[F[A]]): F[List[A]] = fas match {
    case Nil => unit(List[A]())
    case x :: xs => map2(x, sequence(xs))(_ :: _)
  }

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = as match {
    case Nil => unit(List[B]())
    case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))

  /** 12.8
    * Just like we can take the product of two monoids A and B to give the monoid (A, B),
    * we can take the product of two applicative functors. Implement this function:
    */
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val F = this
    new Applicative[({
      type f[x] = (F[x], G[x])
    })#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (F.unit(a), G.unit(a))

      override def map2[A,B,C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
        (F.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }
    }
  }

  /** 12.9
    * Hard: Applicative functors also compose another way! If F[_] and G[_] are applicative
    * functors, then so is F[G[_]]. Implement this function:
    */
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val F = this
    new Applicative[({
      type f[x] = F[G[x]]
    })#f] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

      override def map2[A,B,C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
        F.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
      }
    }
  }

  /** 12.12
    * On the Applicative trait, implement sequence over a Map rather than a List:
    */
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
  ofa.foldRight(unit(Map[K,V]())) {
    case ((k,fv), fMap) =>
      map2(fMap, fv)(_.updated(k, _))
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  override def map[A, B](fa: F[A])(f: (A) => B): F[B] = {
    val unitF = (b: B) => unit(b)
    flatMap(fa)(unitF compose f)
  }

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

object Monad {
  /** 12.5
    * Write a monad instance for Either .
    */
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({
  type f[x] = Either[E, x]
})#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] =
      ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  /** 12.20
    * Hard: Implement the composition of two monads where one of them is traversable.
    */
  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
  Monad[({type f[x] = F[N[x]]})#f] = new Monad[({
    type f[x] = F[N[x]]
  })#f] {
    override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))
    override def flatMap[A, B](ma: F[N[A]])(f: (A) => F[N[B]]): F[N[B]] =
      F.flatMap(ma)(na =>
        F.map(T.traverse(na)(f))(nna =>
          N.join(nna)))
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {
    /** 12.4
      * Hard: What is the meaning of streamApplicative.sequence? Specializing the signa-
      * ture of sequence to Stream, we have this:
      * def sequence[A](a: List[Stream[A]]): Stream[List[A]]
      *
      * Combines the elements of all streams into a list. So first we get a list of the heads of all streams,
      * then the next element and so on...
      */
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  /** 12.6
    * Write an Applicative instance for Validation that accumulates errors in Failure.
    * Note that in the case of Failure there’s always at least one error, stored in head. The
    * rest of the errors accumulate in the tail.
    */
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = new Applicative[({
    type f[x] = Validation[E, x]
  })#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
      f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (Success(a), Success(b)) => unit(f(a, b))
        case (f@Failure(_, _), Success(_)) => f
        case (Success(_), f@Failure(_, _)) => f
        case (Failure(h1, t1), Failure(h2, t2)) =>
          Failure(h1, (h2 +: t1) ++ t2)
      }
    }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  type Identity[A] = A
  implicit val identityApplicative = new Applicative[Identity] {
    override def unit[A](a: => A): Identity[A] = a
    override def apply[A, B](fab: Identity[(A) => B])(fa: Identity[A]): Identity[B] = fab(fa)
  }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  /** 12.14
    * Hard: Implement map in terms of traverse as a method on Traverse[F]. This estab-
    * lishes that Traverse is an extension of Functor and that the traverse function is a
    * generalization of map (for this reason we sometimes call these traversable functors). Note
    * that in implementing map, you can call traverse with your choice of Applicative[G].

    */
  import Applicative.identityApplicative
  def map[A,B](fa: F[A])(f: A => B): F[B] = traverse[Identity, A, B](fa)(f)

  /** 12.15
    * Answer, to your own satisfaction, the question of why it’s not possible for Foldable to
    * extend Functor . Can you think of a Foldable that isn’t a functor?
    *
    * For example a TreeSet (set with a bst implementation) is foldable but not a functor.
    * We can go through the set and combine the result => foldable
    * But when mapping over it with a function (A) => B the type B needs to be orderable so that
    * the tree can be ordered
    *
    * And thus map only works on TreeSet if the new type is orderable (works in Scala, I think all objects are orderable?)
    */


  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  /** 12.16
    * There’s an interesting consequence of being able to turn any traversable functor into
    * a reversed list—we can write, once and for all, a function to reverse any traversable
    * functor! Write this function, and think about what it means for List, Tree, and other
    * traversable functors.
    */
  def reverse[A](fa: F[A]): F[A] = {
    val reversedList = mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2
    // Map through fa and replace values with values from the list
    mapAccum(fa, reversedList) {
      case (_, head :: tail) => (head, tail)
    } ._1
  }

  /** 12.17
    * Use mapAccum to give a default implementation of foldLeft for the Traverse trait.
    */
  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a: A, s: B) => ((), f(s, a)))._2

  /** 12.18
    * Use applicative functor products to write the fusion of two traversals. This function
    * will, given two functions f and g, traverse fa a single time, collecting the results of
    * both functions at once.
    */
  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    implicit val productApplicative = G.product(H)
    traverse[({type f[x] = (G[x], H[x])})#f,A,B](fa)(a => (f(a), g(a)))
  }

  /** 12.19
    * Implement the composition of two Traverse instances.
    */
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = new Traverse[({
    type f[x] = F[G[x]]
  })#f] {
    override def traverse[H[_] : Applicative, A, B](fa: F[G[A]])(f: (A) => H[B]): H[F[G[B]]] = {
      self.traverse(fa) { ga =>
        G.traverse(ga) { a =>
          f(a)
        }
      }
    }
  }

}

object Traverse {
  /** 12.13
    * Write Traverse instances for List, Option, and Tree.
    */
  val listTraverse = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: (A) => G[B]): G[List[B]] = {
      val applicative = implicitly[Applicative[G]]
      fa.foldRight(applicative.unit(List[B]()))((ga, b) => applicative.map2(f(ga), b)(_ :: _))
    }
  }


  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: (A) => G[B]): G[Option[B]] = {
      val applicative = implicitly[Applicative[G]]
      fa match {
        case Some(a) => applicative.map(f(a))(Some.apply)
        case None    => applicative.unit(None)
      }
    }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: (A) => G[B]): G[Tree[B]] = {
      val applicative = implicitly[Applicative[G]]
      applicative.map2(f(fa.head),
        listTraverse.traverse(fa.tail)(traverse(_)(f)))(Tree(_, _))
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}

object TraversableLaws {
  import fpinscala.testing.Gen._

  def reverseLaw[A](ap: Traverse[List], gen: Gen[List[A]]): Prop = {
    Prop.forAll(for {
      x <- gen
      y <- gen
    } yield (x, y)) { case (x, y) =>
    ap.toList(ap.reverse(x)) ++ ap.toList(ap.reverse(y)) ==
    ap.reverse(ap.toList(y) ++ ap.toList(x))}
  }

  def main(args: Array[String]): Unit = {
    val reverseL = reverseLaw(Traverse.listTraverse, listOf(choose(-10, 10))(10))
    Prop.run(reverseL)

    Traverse.optionTraverse.foldLeft(Some(1))("")((s, a) => s + a)
  }
}
