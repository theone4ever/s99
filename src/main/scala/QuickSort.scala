/**
  * Created by theone4ever on 2016-11-30.
  */
import scala.collection.mutable.MutableList
class QuickSort {

  def quickSort(list: MutableList[Int], startIndex: Int, endIndex: Int): MutableList[Int] = {
    val index = partition(list, startIndex, endIndex)
    quickSort(list, startIndex, index-1)
    quickSort(list, index+1, endIndex)
    list
  }

  def partition(list: MutableList[Int], start: Int, end: Int): Int = {
    val pivot = list(start)
    var leftCursor = start+1
    var rightCursor = end
    while(leftCursor<(rightCursor-1)){
      if(list(leftCursor)<=pivot){
        leftCursor +=1
      }else{
        while(list(rightCursor)>pivot&&rightCursor>leftCursor){
          rightCursor-=1
        }
        if(leftCursor < (rightCursor-1)){
          //now we have leftElem> pivot and rightElem<pivot, so swap them
          println(s"Before swap:${list}")
          swap(list, leftCursor, rightCursor)
          println(s"After swap:${list}")
        }
      }
    }
    if(list(leftCursor)<pivot){
      swap(list, leftCursor, start)
      println(s"Return list:${list}")

      println(s"Return index:${leftCursor}")

      leftCursor
    }else{
      swap(list, rightCursor, start)
      println(s"Return list:${list}")
      println(s"Return index:${rightCursor}")
      rightCursor
    }
  }

  def swap(list: MutableList[Int], i1: Int, i2: Int) = {
    val tmp1 = list(i1)
    val tmp2 = list(i2)
    list(i1) = tmp2
    list(i2) = tmp1
    list
  }


  def sort(list: MutableList[Int]): MutableList[Int] = {
    quickSort(list, 0, list.length-1)
  }


}

object HelloWorld {
  def main(args: Array[String]): Unit = {
    val quickSort = new QuickSort()
      println(quickSort.sort(MutableList(5,3,8,4,6,1,2,10,3)))
  }
}
