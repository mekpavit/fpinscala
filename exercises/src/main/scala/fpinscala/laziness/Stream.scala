package fpinscala.laziness

import Stream._
trait Stream[+A] {


  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    // f (x, z) => cons(x+10, z)
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

  def toList: List[A] = 
    this.foldRight(List[A]())((x, result) => x :: result)

  def take(n: Int): Stream[A] = 
    this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
      case _ => Empty
    }

  def drop(n: Int): Stream[A] = 
    this match {
      case Empty => Empty
      case Cons(_, t) if n == 1 => t() 
      case Cons(_, t) if n > 1 => t().drop(n-1)
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }

  def forAll(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) if p(h()) => t().forAll(p)
      case Empty => true
      case _ => false
    }

  def takeWhile2(p: A => Boolean): Stream[A] = 
    this.foldRight(Stream[A]())((x, z) => if (p(x)) cons(x, z) else Stream[A]())

  def headOption: Option[A] = 
    this.foldRight[Option[A]](None)((x, z) => Some(x))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
 
  def map[B](f: A => B): Stream[B] = 
    this.foldRight[Stream[B]](Stream[B]())((x, z) => cons(f(x), z))

  def filter(p: A => Boolean): Stream[A] = 
    this.foldRight[Stream[A]](Stream[A]())((x, z) => if (p(x)) cons(x, z) else z)

  def append[B>:A](s: => Stream[B]): Stream[B] = 
    this.foldRight[Stream[B]](s)((x, z) => cons(x, z))

  def flatmap[B](f: A => Stream[B]): Stream[B] = 
    this.foldRight[Stream[B]](Stream[B]())((x, z) => f(x).append(z))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold[B,Stream[A]](this)({
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None 
  }) 

  def takeViaUnfold(n: Int): Stream[A] = unfold[A,Tuple2[Stream[A],Int]]((this, n))({
    case (Cons(h, t), i) if i > 0 => Some((h(), (t(), i-1)))
    case _ => None
  })

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this)({
    case Cons(h, t) => if (p(h())) Some((h(), t())) else None
    case Empty => None
  })

  def zipWith[B,C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s))({
    case (Cons(h1 ,t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2))({
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
    case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
    case _ => None
  })

  def startsWith[A](s: Stream[A]): Boolean = 
    this.zipWith(s)(_ == _).forAll(b => b)

  def tails: Stream[Stream[A]] = 
    cons(this, unfold(this)({
      case Cons(h, t)  => t() match {
        case tt @ Cons(_, _) => Some((tt, tt))
        case _ => None
      } 
      case Empty => None
    }))

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = this.foldRight(Stream(z))((x, res) => {
    res match {
      case Cons(h, _) => cons(f(x, h()), res)
    }
  })

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
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
  def fibs: Stream[Int] = {
    def fib(f1: Int, f2: Int): Stream[Int] = Stream.cons(f1+f2, fib(f1+f2, f1))
    Stream.cons(0, fib(0, 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some(value) => cons(value._1, unfold(value._2)(f))
  }

  def fibsViaUnfold: Stream[Int] = cons(0, cons(1, unfold[Int,Tuple2[Int,Int]]((0, 1))(s => Some(s._1 + s._2, (s._2, s._1 + s._2)))))

  def fromViaUnfold(n: Int): Stream[Int] = unfold[Int,Int](n-1)(s => Some((s+1, s+1)))
  
  def constantViaUnfold[A](a: A): Stream[A] = unfold[A,A](a)(s => Some((s, s)))

  val onesViaUnfold: Stream[Int] = unfold[Int,Int](1)(s => Some((1, 1)))

}
