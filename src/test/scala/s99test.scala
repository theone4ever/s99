package test.scala
import main.scala.s99._
import org.scalatest.FunSuite


/**
  * Created by theone4ever on 22/06/16.
  */
class s99test extends FunSuite{
  test("last(list)") {
    assert(last(List(1,2,3)).get == 3)
  }

  test("nth") {
    assert(nth(2, List(1, 1, 2, 3, 5, 8)).get == 2)
  }

  test("length"){
    assert(length(List(1, 1, 2, 3, 5, 8)) == 6)
  }

  test("penultimate"){
    assert(penultimate(List(1,2,3,4)).get == 3)
  }

  test("reverse"){
    assert(reverse(List(1, 1, 2, 3, 5, 8)) == List(8,5,3,2,1,1))
  }

  test("isPalindrome"){
    assert(isPalindrome(List(1, 2, 3, 2, 1)) == true)
  }

  test("flatten"){
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1,1,2,3,5,8))
  }

}