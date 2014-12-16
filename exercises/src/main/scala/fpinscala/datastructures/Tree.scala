package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // See answers
//  def fold[A, B](tree: Tree[A])(leafConversion: A => B)(branchConversion: (B, B) => B): B = ???
//
//  def sizeViaFold[A](tree: Tree[A]) = fold(tree)(x => 1)(1 + _ + _)

  def map[A](tree: Tree[A])(f: A => A): Tree[A] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }
}

object TestTree {
  def main(args: Array[String]) = {
    println("Size", Tree.size(Leaf(1)) == 1)

    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(4)))
    println("Size", Tree.size(tree) == 5)

    println("Maximum leaf", Tree.maximum(Leaf(5)) == 5)
    println("Maximum tree", Tree.maximum(tree) == 4)

    println("Depth leaf", Tree.depth(Leaf(1)) == 0)
    println("Depth tree", Tree.depth(tree) == 2)

    println("Map leaf", Tree.map(Leaf(2))(_ + 1))
    println("Map tree", Tree.map(tree)(_ + 1))

//    println("Size via fold", Tree.sizeViaFold(tree) == 5)
  }
}