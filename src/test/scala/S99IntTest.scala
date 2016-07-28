import arithmetic.S99Int._
import org.scalatest.FunSuite

/**
  * Created by eqqiwng on 7/27/16.
  */
class S99IntTest extends FunSuite{
  test("isPrime") {
    assert(7.isPrime)
    assert(12.isPrime == false)
  }

  test("gcd") {
    assert(gcd(36, 63) == 9)
    assert(gcd(3, 2) == 1)
    assert(gcd(3, 1) == 1)
    assert(gcd(10, 2) == 2)
  }

  test("isCoprimeTo"){
    assert(35.isCoprimeTo(64))
    assert(!36.isCoprimeTo(64))
    assert(!2.isCoprimeTo(10))
    assert(1.isCoprimeTo(10))
  }

  test("totient"){
    assert(10.totient == 5)
  }

  test("primeFactors"){
    assert(315.primeFactors == List(7,5,3,1))
  }
}
