package fpinscala.ch03datastructures

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

  def append[A](a1: List[A], a2: List[A]): List[A] =
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


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(h, t) => t
    case _ => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(o, t) => Cons(h, t)
    case _ => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case m => drop(tail(l), m - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) | Nil => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((el, res) => res + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((res, _) => res + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((res, el) => Cons(el, res))

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    def g: (A, B) => B = (a, b) => f(b, a)
    foldRight(reverse(l), z)(g)
  }

  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    def g: (B, A) => B = (b, a) => f(a, b)
    foldLeft(reverse(l), z)(g)
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def listOfLists[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil:List[A])((res, el) => append(res, el))
//  foldRight(l, Nil:List[A])((el, res) => append(el, res))

  def addOne(l: List[Int]): List[Int] = l match {
    case Cons(h, t) => Cons(h + 1, addOne(t))
    case _ => Nil
  }

  def listToString(l: List[Double]): List[String] = l match {
    case Cons(h, t) => Cons(h.toString, listToString(t))
    case _ => Nil
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Cons(h, t) => Cons(f(h), map(t)(f))
    case _ => Nil
  }

  def filter_firstVersion[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
    case _ => Nil
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Cons(h, t) => append(f(h), flatMap(t)(f))
    case _ => Nil
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else List())

  def addLists(al: List[Int], bl: List[Int]): List[Int] = (al, bl) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
    case (l1, Nil) => l1
    case (Nil, l2) => l2
  }

  def zipWith[A](al: List[A], bl: List[A])(f: (A, A) => A): List[A] = (al, bl) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case (l1, Nil) => l1
    case (Nil, l2) => l2
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def isSubsequence(sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (Cons(hsup, tsup), Cons(hsub, tsub)) => hsup == hsub && isSubsequence(tsup, tsub)
      case (_, Nil) => true
      case _ => false
    }

    sup != Nil && (isSubsequence(sup, sub) || hasSubsequence(tail(sup), sub))
  }

}
