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
      if (start == 1) List(1)
      else if (start == 2) List(1, 2)
      else (1 to start / 2 reverse).filter(start % _ == 0).toList
    }

    def primeFactors: List[Int] = {
      factors.filter(_.isPrime)
    }

    /**
      * P40, Goldbach's conjecture.
      *
      * @return
      */
    def goldbach: (Int, Int) = {
      assert(start > 2 && start % 2 == 0)

      val primerList = listPrimesinRange(2 to start)
      find2numbers(start, primerList)
    }

    def find2numbers(sum: Int, list: List[Int]): (Int, Int) = {
      list match {
        case Nil=> (0,0)
        case h::t=> {
          if(t.contains(sum-h)) (h, sum-h)
          else
            find2numbers(sum, t)
        }
      }
    }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    def gcd(one: Int, two: Int): Int = {
      one.factors.find(two.factors.contains(_)).get
    }

    /**
      * P30.  A list of prime numbers.
      * @param range
      * @return
      */
    def listPrimesinRange(range: Range): List[Int]= {
      range.foldLeft(List[Int]())((result, num)=> {
        if(num.isPrime)
          num::result
        else
          result
      }).sortWith((a,b)=> a<b)
    }

  }

}
