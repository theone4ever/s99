package test.scala
import main.scala.S99._
import org.scalatest.FunSuite


/**
  * Created by theone4ever on 22/06/16.
  */
class S99Test extends FunSuite{
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

  test("compress"){
    assert(compress(List('a', 'b', 'b', 'c', 'c')) == List('a', 'b', 'c'))

    assert(compress(List('a', 'a', 'b', 'b', 'c', 'c')) == List('a', 'b', 'c'))

    assert(compress(Nil) == Nil)

    assert(compress(List('a', 'a', 'a')) == List('a'))
  }

  test("untilDiff"){
    print(untilDiff('a', List('a', 'a', 'a', 'a', 'b')))
  }

  test("pack"){
    assert(pack(List('a', 'a',  'b', 'b', 'c')) ==List(List('a', 'a'), List('b', 'b'), List('c')) )
    assert(pack(List('a',  'b', 'b', 'c')) ==List(List('a'), List('b', 'b'), List('c')) )
    assert(pack(List('a',  'b', 'b', 'c', 'c')) ==List(List('a'), List('b', 'b'), List('c', 'c')) )
    assert(pack(Nil) ==  Nil)
  }

  test("drop"){
    assert(drop(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) == List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k'))
  }

  test("slice"){
    assert(slice(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) == List('d', 'e', 'f', 'g'))
  }

  test("combinations"){
    assert(combinations(3, List('a', 'b', 'c', 'd')).length == 4)
    assert(combinations(4, List('a', 'b', 'c', 'd')).length == 1)
    assert(combinations(2, List('a', 'b', 'c', 'd')).length == 6)
    assert(combinations(1, List('a', 'b', 'c', 'd')).length == 4)
  }

  test("lsort"){
    print(lsort(List(List('a', 'b', 'c'), List('d', 'e'), List('f', 'g', 'h'), List('d', 'e'), List('i', 'j', 'k', 'l'), List('m', 'n'), List('o'))))
  }

  test("lsortFeq"){
    print(lsortFeq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))
  }

}
