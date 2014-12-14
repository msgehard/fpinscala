package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("Can't take tail of empty list")
      case Cons(_, tail) => tail
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, tail) => Cons(h, tail)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) return l
    l match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n - 1)
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      // Note the use of conditional in the pattern match
      case Cons(h, t) if (f(h)) => dropWhile(t)(f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((x, acc) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc, x) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, x) => Cons(x, acc))

  def appendFoldLeft[A](l1: List[A], l2: List[A]): List[A] = foldLeft(reverse(l1), l2)((acc, x) => Cons(x, acc))
  def appendFoldRight[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((x, acc) => Cons(x, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((x, acc) => Cons(f(x), acc))
  // This one doesn't work. The resulting list is in reverse order. Why?
  // I think this is because foldLeft starts at the left and appends to acc where
  // foldRight starts at the end and adds the new values at the end.
//  def map[A,B](l: List[A])(f: A => B): List[B] = foldLeft(l, List[B]())((acc, x) => Cons(f(x), acc))

  def filter[A](l: List[A])(include: A => Boolean) = foldRight(l, List[A]())((x, acc) => if (include(x)) Cons(x, acc) else acc )
  def filterViaFlatMap[A](l: List[A])(include: A => Boolean) = flatMap(l)((x) => if (include(x)) List(x) else Nil)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, List[B]())((x, acc) => append(f(x), acc))

  def addElements(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addElements(t1, t2))
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

}

object TestList {
  def main(args: Array[String]): Unit = {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println("Pattern match", x)

    println("Tail", List.tail(List(1, 2, 3, 4, 5)))
//    println("Tail Nil", List.tail(Nil))
//    println("Tail List()", List.tail(List()))

    println("Set head List()", List.setHead(List(), 2))
    println("Set head ", List.setHead(List(1,2), 6))

    println("Drop", List.drop(List(1,2,3,4,5), 3))
    println("Drop Nil", List.drop(Nil, 3))

    println("Drop while Nil", List.dropWhile(Nil)((x:Int) => x < 5))
    println("Drop while", List.dropWhile(List(1,2,3,4,5))(x => x < 2))

    println("Init", List.init(List()))
    println("Init", List.init(List(1)))
    println("Init", List.init(List(1,2,3,4)))

    println("Length", List.length(Nil))
    println("Length", List.length(List(1,2)))

    println("FoldLeft", List.foldLeft(List(1,2,3), 0)((acc, x) => acc + 1))
    println("LengthLeft", List.lengthLeft(List(1,2,3,6)))

    println("Reverse", List.reverse(List(1,2,3,6)))

    println("Append", List.append(List(1,2,3), List(4,5,6)))
    println("AppendFoldLeft", List.appendFoldLeft(List(1,2,3), List(4,5,6)))
    println("AppendFoldRight", List.appendFoldRight(List(1,2,3), List(4,5,6)))

    println("Map", List.map(List(1,2,3))(_ + 1))

    println("Filter", List.filter(List(1,2,3))(_ > 2))

    println("FlatMap", List.flatMap(List(1,2,3))(i => List(i,i)))

    println("FilterViaFlatMap", List.filterViaFlatMap(List(1,2,3))(_ > 2))

    println("AddElements", List.addElements(List(1,2,3), List(4,5,6)))

    println("Zip with", List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _))

  }
}