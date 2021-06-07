package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v)      => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def depth[A](t: Tree[A]): Int = {
    def loop(t: Tree[A], currentDepth: Int): Int = t match {
      case Leaf(_)      => currentDepth
      case Branch(l, r) => loop(l, currentDepth+1).max(loop(r, currentDepth+1))
    }
    loop(t, 0)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _)

  def maximum2(t: Tree[Int]): Int = fold(t)(v => v)(_.max(_))

  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 0)(1 + _.max(_))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)((a: A) => Leaf(f(a)): Tree[B])(Branch(_, _))

}
