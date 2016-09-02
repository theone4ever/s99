import org.scalatest.FunSuite
import logic.S99Logic._

/**
  * Created by eqqiwng on 8/1/16.
  */
class S99LogicTest extends FunSuite{
  test("hello") {
    assert(true)
  }

  test("and"){
    assert(and(true, true))
    assert(!xor(true,true))
    assert(nand(false, false))
    assert(nand(false, true))
    assert(nand(true, false))
    assert(!nand(true, true))

    assert(table2(true, true))
    assert(table2(true, false))
    assert(!table2(false, true))
    assert(!table2(false, false))

    assert(true.and(false) == false)

  }

  test("gray"){
    print(gray(3))
  }


}