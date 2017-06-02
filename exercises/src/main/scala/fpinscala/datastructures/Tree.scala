package fpinscala.datastructures
import scala.math.max

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(value) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => depth(l) max depth(r)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(l) => Leaf(f(l))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(l) => f(l)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def foldMap[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(l => Leaf(f(l)): Tree[B])(Branch(_, _))
}