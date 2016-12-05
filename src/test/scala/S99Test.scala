package test.scala
import main.scala.S99._
import main.scala.{BinaryTree, TreeNode}
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


  test("encode"){
    assert(encode(List('a', 'a',  'b', 'b', 'c')) ==List((2, 'a'), (2, 'b'), (1, 'c')) )
    assert(pack(Nil) ==  Nil)
  }

  test("encodeModified"){
    assert(encodeModified(List('a', 'a',  'b', 'b', 'c')) ==List((2, 'a'), (2, 'b'), 'c'))
    assert(pack(Nil) ==  Nil)
  }

  test("decode"){
    assert(decode(List((4,'a'), (2,'b'), (1, 'c'))) ==List('a', 'a', 'a','a', 'b', 'b', 'c'))
  }


  test("encodeDirect"){
    assert(encodeDirect(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) == List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e')))
  }

  test("duplicate"){
    assert(duplicate(List('a', 'b', 'c')) == List('a', 'a', 'b', 'b', 'c', 'c'))
  }

  test("duplicateN"){
    assert(duplicateN2(List('a', 'b', 'c'), 2) == List('a', 'a', 'b', 'b', 'c', 'c'))

  }


  test("bfs"){
    val tree = TreeNode(8,9,10)
    val n1 = TreeNode(7)
    val n2 = TreeNode(6)
    val n3 = TreeNode(5)
    val n4 = TreeNode(4)
    tree.left.left = n1
    tree.left.right = n2
    tree.right.left = n3
    tree.right.right = n4
    println(tree)

    assert(BinaryTree.bfs(tree, 3).getOrElse(null) == n4)


  }



  test("maxLevel"){
    val tree = TreeNode(8,9,10)
    val n1 = TreeNode(7)
    val n2 = TreeNode(6)
    val n3 = TreeNode(5)
    tree.left.left = n1
    tree.left.right = n2
    tree.right.left = n3
    println(tree)
    println(BinaryTree.maxLevel(tree))
  }

  test("printPath2Leaf"){
    val tree = TreeNode(8,9,10)
    val n1 = TreeNode(7)
    val n2 = TreeNode(6)
    val n3 = TreeNode(5)
    val n4 = TreeNode(4)
    tree.left.left = n1
    tree.left.right = n2
    tree.right.left = n3
    tree.right.right = n4
    BinaryTree.printPath2Leaf(tree, List())
  }


>>>>>>> some more finished:src/test/scala/s99test.scala
}
