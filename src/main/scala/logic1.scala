package logic {

  import scala.collection.mutable

  class S99Logic(val start: Boolean) {
    def and(b: Boolean): Boolean = S99Logic.and(start, b)


  }

  object S99Logic {
    implicit def boolean2S99Logic(i: Boolean): S99Logic = new S99Logic(i)

    def and(arg1: Boolean, arg2: Boolean) = arg1 && arg2

    def or(arg1: Boolean, arg2: Boolean) = arg1 || arg2

    def xor(arg1: Boolean, arg2: Boolean) = arg1 != arg2

    def nand(arg1: Boolean, arg2: Boolean) = !(arg1 && arg2)

    def nor(arg1: Boolean, arg2: Boolean) = !or(arg1, arg2)

    def impl(arg1: Boolean, arg2: Boolean) = ???

    def table2(a: Boolean, b: Boolean) = a && or(a, b)



    def gray0(n: Int): List[String] = {
      if (n < 1) Nil
      else if (n > 1) gray(n - 1).map("0" + _) ::: gray(n - 1).map("1" + _)
      else List[String]("0", "1")
    }

    def mem[A, B](f: A=>B) = new Function[A,B]{
      private val cache = new mutable.HashMap[A, B]
      def apply(a: A): B = {
        cache.getOrElseUpdate(a, f(a))
      }
    }

    /**
      * P49, gray code with memorization.
      */
    val gray = mem(gray0)
  }

}