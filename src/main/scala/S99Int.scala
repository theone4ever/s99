package arithmetic {

  class S99Int(val start: Int) {

    import S99Int._

    def isPrime: Boolean = {
      (2 until start filter (n => start % n == 0) length) == 0
    }

    def isCoprimeTo(other: Int) = {
      gcd(start, other) == 1
    }

    def totient = {
      val array = (1 to start filter (start.isCoprimeTo(_)))
      print(array)
      array.length
    }

    def factors: List[Int] = {
      if(start ==1) List(1)
      else if(start == 2) List(1,2)
      else (1 to start/2 reverse).filter(start %_ == 0).toList
    }

    def primeFactors: List[Int] = {
      factors.filter(_.isPrime)
    }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    def gcd(one: Int, two: Int): Int = {
        one.factors.find(two.factors.contains(_)).get
    }
  }

}
