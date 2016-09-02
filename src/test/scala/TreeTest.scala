package Tree
import org.scalatest.FunSuite
import tree.{Node, _}
import tree.Tree.cBalanced
/**
  * Created by eqqiwng on 8/2/16.
  */
class TreeTest extends FunSuite{

  test("addBalance"){
    assert(cBalanced(4, "c").size== (Math.pow(2, 4-1)).toInt)
  }
}
