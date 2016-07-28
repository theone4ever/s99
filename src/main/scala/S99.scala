package main.scala

/**
  * Created by theone4ever on 22/06/16.
  */
object S99 {
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

  def compress[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case h :: Nil => List(h)
    case h :: t => {
      t match {
        case h1 :: t1 if (h == h1) => compress(t)
        case h1 :: t1 if (h != h1) => h :: compress(t1)
      }

    }
  }


  def pack[A](list: List[A]): List[List[A]] = list match {
    case Nil => Nil
    case h :: Nil => List(List(h))
    case h :: t => {
      t match {
        case h1 :: t1 if (h == h1) => {
          val (l1, t1) = untilDiff(h, t)
          l1 :: pack(t1)
        }
        case h1 :: t1 if (h != h1) => List(h) :: pack(t)
      }
    }
  }

  def untilDiff[A](h: A, t: List[A]): (List[A], List[A]) = t match {
    case Nil => (List(h), Nil)
    case h1 :: t1 if (h == h1) => {
      val (l1, l2) = untilDiff(h1, t1)
      (h :: l1, l2)
    }
    case h1 :: t1 if (h != h1) => {
      (List(h), t)
    }
  }


  /**
    * P16
    *
    * @param n
    * @param list
    * @tparam A
    * @return
    */
  def drop[A](n: Int, list: List[A]): List[A] = {
    drop2(n, 1, list)
  }

  def drop2[A](n: Int, l: Int, list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case h :: t => {
        if (l % n == 0) {
          drop2(n, l + 1, t)
        } else {
          h :: drop2(n, l + 1, t)
        }
      }
    }
  }

  /**
    * TODO: P17: too easy
    */


  /**
    * P18: Extract a slice from a list.
    *
    * @param start
    * @param end
    * @param list
    * @tparam A
    * @return
    */
  def slice[A](start: Int, end: Int, list: List[A]): List[A] = {
    slice2(start, end, 0, list)
  }

  def slice2[A](start: Int, end: Int, l: Int, list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case h :: t => {
        if (l < start)
          slice2(start, end, l + 1, t)
        else if (l >= start && l < end)
          h :: slice2(start, end, l + 1, t)
        else
          Nil
      }
    }
  }


  /**
    * P26: Generate the combinations of K distinct objects chosen from the N elements of a list.
    *
    * @param n
    * @param list
    * @tparam A
    * @return
    */
  def combinations[A](n: Int, list: List[A]): List[List[A]] = {
    if (n > list.length)
      Nil
    else if (n == list.length)
      List[List[A]](list)
    else {
      list match {
        case Nil => Nil
        case h :: t => {
          if (n > 1)
            combinations(n - 1, t).map(h :: _) ::: combinations(n, t)
          else if (n == 1) {
            List(h) :: combinations(n, t)
          }
          else
            Nil
        }
      }
    }
  }

  /**
    * P27 a: Group the elements of a set into disjoint subsets. Group 9 people into 3 groups with 2,3,4 people
    * @param list
    * @tparam A
    * @return
    */
  def group3[A](list: List[A]): List[List[A]] = {

    ???
  }


  /**
    * P28 a: Sorting a list of lists according to length of sublists.
    * @param list
    * @tparam A
    * @return
    */
  def lsort[A](list: List[List[A]]): List[List[A]] = {
    list.sortWith((l1, l2) => l1.length<=l2.length)
  }

  /**
    * P28 b: Sorting a list of lists according to length of sublists, by frequency of length
    * @param list
    * @tparam A
    * @return
    */
  def lsortFeq[A](list: List[List[A]]): List[List[A]] = {
    val listLen = list.map(l=>(l, l.length))
    val freqMap = scala.collection.mutable.Map[Int, Int]()
    listLen.foreach(len=>freqMap.put(len._2, freqMap.getOrElse(len._2, 0)+1 ))
    listLen.map(elem=>(elem._1, freqMap.getOrElse(elem._2, -1)))
      .sortWith((l1: (List[A], Int), l2: (List[A], Int))=>l1._2<=l2._2).
      map(_._1)
  }

}
