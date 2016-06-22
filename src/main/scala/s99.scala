package main.scala

/**
  * Created by theone4ever on 22/06/16.
  */
object s99 {
  def last[A](list: List[A]): Option[A] =
    list match {
      case Nil => None
      case h :: Nil => Some(h)
      case h :: t => last(t)
    }


  def nth[A](n: Int, list: List[A]): Option[A] = list match {
    case Nil => None
    case h :: Nil => if (n == 0) Some(h) else None
    case h :: t => if (n == 0) Some(h) else nth(n - 1, t)
  }

  def length[A](list: List[A]): Int = list match {
    case Nil => 0
    case h :: t => 1 + length(t)
  }


  def penultimate[A](list: List[A]): Option[A] =
    nth(length(list) - 2, list)


  def reverse[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case h :: Nil => List(h)
    case h :: t => reverse(t) :+ h
  }


  def isPalindrome[A](list: List[A]): Boolean = {
    val reversed = reverse(list)
    //  println(reversed)
    compare(list, reversed)
  }


  def compare[A](l1: List[A], l2: List[A]): Boolean = (l1, l2) match {
    case (Nil, Nil) => println("nil"); true
    case (h1 :: t1, h2 :: t2) =>
      if (h1.equals(h2)) compare(t1, t2)
      else false
  }


  def flatten[A](list: Any): List[Any] = list match {
    case Nil => Nil
    case h :: t => h match {
      case h1 :: t2 => flatten(h) ::: flatten(t)
      case _ => h :: flatten(t)
    }
  }

}
