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
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    def gcd(one: Int, two: Int): Int = {
      if (one == 1 || two == 1)
        1
      else if(one == 2){
        if(two%2 == 0) 2
        else 1
      }else if( two == 2){
        if( one%2 == 0) 2
        else 1
      }
      else {
        val oneDividors = (1 to one / 2 reverse).filter(one % _ == 0)
        val twoDividors = (1 to two / 2 reverse).filter(two % _ == 0)
        oneDividors.find(twoDividors.contains(_)).get
      }
    }


  }

}
