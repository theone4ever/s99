import scala.collection.mutable

/**
  * Created by theone4ever on 01/07/16.
  */
trait Node{
  def id: Int
  def pId: Int
}

case class Leaf(id: Int, pId: Int) extends Node
case class Folder(id: Int, pId: Int, children: mutable.MutableList[Node] = new mutable.MutableList[Node]) extends Node{
  def addChild(n: Node)={
    children.+=(n)
  }
}



object Main {
  def listToForest(list: List[Node]): List[Node] = {
    val id2node: Map[Int, Node] = list.map(n=>n.id->n).toMap
    list.foreach(node=>{
      if(node.pId!= -1){
        id2node(node.pId).asInstanceOf[Folder].addChild(node)
      }
    })
    id2node.values.filter(_.pId == -1).toList
  }
  def main(args: Array[String]): Unit = {
    val l = List[Node](Leaf(10,9), Folder(9, 8), Folder(8,7), Folder(7,6), Folder(6,-1), Leaf(4, 3), Folder(3,2), Folder(2,-1))
//    print(l.map(n=>n.id->n).toMap)
    print(listToForest(l))
  }
}






