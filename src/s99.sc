def last(list: List[Any]): Any =
  list match {
    case Nil=> Nil
    case h::Nil => h
    case h::t => last(t)
  }



last(List(1,2,3))
last(Nil)

def  penultimate(list: List[Any]) : Any = list match {
  case Nil=> Nil
  case h::t =>  penultimate2(h, t)
}

def  penultimate2(result: Any, list: List[Any]) : Any = list match {
  case Nil=> Nil
  case h::Nil => result
  case h::t=>  penultimate2(h, t)
}


penultimate(List(1,2,3,4))


def nth(n: Int, list: List[Any]): Any = list match {
  case Nil=> Nil
  case h::Nil=> if(n==0) h else Nil
  case h::t=> if(n==0) h else nth(n-1, t)
}


nth(2, List(1, 1, 2, 3, 5, 8))

def length(list: List[Any]): Int = list match {
  case Nil => 0
  case h:: t=> 1+length(t)
}


length(List(1, 1, 2, 3, 5, 8))



def reverse(list: List[Any]): List[Any] = list match {
  case Nil=> Nil
  case h::Nil=> List(h)
  case h::t=> reverse(t) :+h
}

reverse(List(1, 1, 2, 3, 5, 8))


def isPalindrome(list: List[Any]): Boolean = {
  val reversed = reverse(list)
//  println(reversed)
  compare(list, reversed)
}


def compare(l1: List[Any], l2: List[Any]): Boolean = l1 match {
  case Nil => println("nil"); true
  case h :: t =>
    if (h.equals(l2.head)) compare(t, l2.tail)
    else false
}

isPalindrome(List(1, 2, 3, 2, 1))
