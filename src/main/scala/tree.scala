package tree

/**
  * Created by eqqiwng on 8/2/16.
  */
sealed abstract class Tree[+T] {
  def count(): Int = this match {
    case n: Node[T] => 1 + n.left.count() + n.right.count()
    case End => 0
  }

  def addBalance[T](value: T): List[Tree[T]] = this match {
    case End => List(Node(value))
    case Node(x: T, End, End) => List(Node(x, Node(value), End), Node(x, End, Node(value)))
    case Node(x: T, y: Node[T], End) => List(Node(x, y, Node(value)))
    case Node(x: T, End, y: Node[T]) => List(Node(x, Node(value), y))
    case Node(x: T, left: Node[T], right: Node[T]) => {
      if (left.count() > right.count()) right.addBalance(value).map(Node(x, left, _))
      else if (left.count() < right.count) left.addBalance(value).map( Node(x, _, right))
      else left.addBalance(value).map(Node(x, _, right)) ::: right.addBalance(value).map( Node(x, left, _))
    }
  }


}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

case object End extends Tree[Nothing]

object Node {
  def apply[T](t: T): Tree[T] = Node(t, End, End)
}

object Tree {
  def cBalanced[T](n: Int, value: T): List[Tree[T]] = n match {
    case x if x <= 0 =>Nil
    case 1 => List(Node(value))
    case x => {
      cBalanced(n-1, value).foldLeft(List[Tree[T]]())((accu, tree)=> tree.addBalance(value):::accu)
    }
  }
}
