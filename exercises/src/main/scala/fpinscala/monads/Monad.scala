package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]()))((a, list) => map2(a, list)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List[B]()))((a, list) => map2(f(a), list)(_ :: _))

  /* For lists -> list of lists
   * For options -> Some generates Some(list of length n containing a), None generates None
   */
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(List[A]()))((a, list) => {
      flatMap(f(a))(b => {
        if (b) map2(unit(a), list)(_ :: _)
        else list
      })
    })

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    (a) => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => unit(ma), f)()


  /** 11.9
    * Show that the two formulations of the associative law, the on in terms of `flatMap` and the one
    * in terms of `compose` are equivalent
    *
    * flatMap: x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
    * compose: compose(compose(f, g), h) == compose(f, compose(g, h))
    *
    * Start with compose law and insert compose definition in terms of flatMap:
    * (b => (a => f(a).flatMap(g))(b).flatMap(h)) == (b => f(b).flatMap(a => g(a).flatMap(h)))
    *
    * Simplify:
    * (b => f(b).flatMap(g).flatMap(h)) == (b => f(b).flatMap(a => g(a).flatMap(h)))
    *
    * f(y) == x
    * (b => f(b).flatMap(g).flatMap(h))(y) == (b => f(b).flatMap(a => g(a).flatMap(h)))(y)
    *
    * Simplify:
    * x.flatMap(g).flatMap(h) == x.flatMap(a => g(a).flatMap(h))
    * OK!
    */

  /** 11.10
    * Prove that these two statements of the identity laws are equivalent
    *
    * compose:
    * compose(f, unit) == f
    * compose(unit, f) == f
    *
    * flatMap:
    * flatMap(x)(unit) == x
    * flatMap(unit(y))(f) == f(y)
    *
    * Start with compose law and insert compose definition in terms of flatMap
    * (a => f(a).flatMap(unit)) == f
    *
    * f(y) == x =>
    * (a => f(a).flatMap(unit))(y) == f(y)
    * f(y).flatMap(unit) == f(y)
    * flatMap(x)(unit) == x
    *
    * Other side:
    * (a => unit(a).flatMap(f)) == f
    *
    * f(y) == x =>
    * (a => unit(a).flatMap(f))(y) == f(y)
    * unit(y).flatMap(f) == f(y)
    * flatMap(unit(y))(f) == x
    */

  /** 11.11
    * Prove that the identity holds for a monad of your choice
    *
    * For Option:
    * flatMap(x)(unit) == x
    * flatMap(Some(a))(b => Some(b)) == Some(a)
    * Definition of flatMap =>
    * Some(a) == Some(a)
    *
    * flatMap(unit(y))(f) == f(y)
    * flatMap(Some(y))(f) == f(y)
    * Definition of flatMap =>
    * f(y) == f(y)
    *
    * None:
    * flatMap(None)(unit) == None
    * None == None
    *
    */

  /** 11.14
    * Monad laws in terms of join, map and unit
    * join(map(map(x)(f))(g)) == join(map(x)(a => join(map(f(a))(g))
    * join(map(x)(unit)) == x
    * join(map(unit(y))(f) == f(y)
    *
    */



  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(a => a)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)
    def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] =
      ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] =
      ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: (A) => State[S, B]): State[S, B] = flatMap(st)(f)
  }

  /** 11.18
    * The meaning of replicateM in stateMonad?
    * applies a computation M times and collects the result into a list
    *
    * How does map2 behave
    * applies the first stateful computation then the second and the applies f to the results
    *
    * Sequence?
    * applies a list of stateful computations and collects the result
    */

  /** 11.19
    * Laws for getState, setState, unit, flatMap
    * state.getState == state.flatMap(unit).getState
    * state.setState(s).getState == state.setState(s).flatMap(unit).getState    *
    * TODO more
    */

  val idMonad: Monad[Id] = new Monad[Id] {
    def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)
    def unit[A](a: => A): Id[A] = Id(a)
  }

  def readerMonad[R] = Reader.readerMonad

  /** 11.20
    * Reader
    *
    * What does it mean?
    * A function that produces a value. It can't create or modify R so it has to be the same
    *
    * What are its primitive operations?
    * Reader(R) returns a value
    *
    * What is the action of flatMap?
    * The result of calling the first reader with R is fed into a function f that produces a new reader.
    * It is then called with the same R
    *
    * What meaning does it give to monadic functions like `sequence`, `join` and `replicateM`?
    * `sequence` takes a list of Readers and returns a Reader that returns a list of the results of all
    * the original readers
    *
    * `join` takes a reader that produces a reader and returns a reader that calls the second reader "directly"
    *
    * `replicateM` creates a new Reader that calls the original reader n times and returns a list of the results
    *
    * What meaning does it give to the monad laws?
    * TODO?
    * Associative law
    * Doesn't matter which order the readers are put
    *
    * Identity law
    * Produces the same value
    */
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader({ _: R => a })
    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      Reader(r => f(st.run(r)).run(r))
  }
}
