package fpinscala.datastructures

import scala.collection.mutable.ListBuffer

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil
    extends List[
      Nothing
    ] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](
      as: List[A],
      z: B
  )(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil        => sys.error("tail of empty list")
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil           => Cons(h, Nil)
    case Cons(_, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil        => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      if (!f(h)) l
      else dropWhile(t, f)
    }
  }

  def init[A](l: List[A]): List[A] = {
    val buf = ListBuffer[A]()
    @annotation.tailrec
    def loop(l: List[A]): Unit = l match {
      case Nil          => sys.error("got an empty list")
      case Cons(h, Nil) => Unit
      case Cons(h, t)   => buf += h; loop(t);
    }
    loop(l)
    apply(buf: _*)
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, count) => count + 1)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((x, z) => f(z, x))

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((z, x) => f(x, z))

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((z, x) => Cons(x, z))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def appendLists[A](lists: List[List[A]]): List[A] =
    foldRight(lists, Nil: List[A])((l, z) => append(l, z))

  def incrementByOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((x, z) => Cons(x + 1, z))

  def listOfDoubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((x, z) => Cons(x.toString(), z))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, z) => Cons(f(x), z))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((x, z) => if (f(x)) Cons(x, z) else z)

  def removeOddNumbers(l: List[Int]): List[Int] = filter(l)(_ % 2 == 0)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((x, z) => append(f(x), z))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((x) => if (f(x)) Cons(x, Nil) else List[A]())

  def addTwoList(l1: List[Int], l2: List[Int]): List[Int] = {
    l1 match {
      case Nil => Nil
      case Cons(head1, tail1) =>
        l2 match {
          case Nil => Nil
          case Cons(head2, tail2) =>
            Cons(head1 + head2, addTwoList(tail1, tail2))
        }
    }
  }

  def zipWith[A, B](l1: List[A], l2: List[B]): List[Tuple2[A, B]] = {
    val buf = ListBuffer[Tuple2[A, B]]()
    def loop(l1: List[A], l2: List[B]): Unit = (l1, l2) match {
      case (Nil, _) => Unit
      case (_, Nil) => Unit 
      case (Cons(h1, t1), Cons(h2, t2)) => buf += Tuple2[A, B](h1, h2); loop(t1, t2)
    }
    loop(l1, l2)
    List(buf:_*)
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
}
