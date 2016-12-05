package main.scala

import scala.collection.mutable

/**
  * Created by theone4ever on 2016-12-04.
  */


import scala.collection.mutable


case class TreeNode(var left: TreeNode, var right: TreeNode, value: Int)


object TreeNode {
  def apply(value: Int): TreeNode = {
    new TreeNode(null, null, value)
  }

  def apply(leftValue: Int, rightValue: Int, value: Int): TreeNode = {
    val tree = new TreeNode(null, null, value)
    if (leftValue > 0) {
      tree.left = new TreeNode(null, null, leftValue)
    }
    if (rightValue > 0) {
      tree.right = new TreeNode(null, null, rightValue)
    }
    tree
  }
}


object BinaryTree {


  def addChildren(node: TreeNode, queue: mutable.Queue[TreeNode]): mutable.Queue[TreeNode] = {
    if (node.left != null) {
      queue.enqueue(node.left)
    }
    if (node.right != null) {
      queue.enqueue(node.right)
    }
    queue
  }

  def addChildrenWithLevel(node: TreeNode, level: Int, queue: mutable.Queue[(TreeNode, Int)]): Int = {
    var numOfNodesAdded = 0
    if (node.left != null) {
      queue.enqueue((node.left, level + 1))
      numOfNodesAdded+=1
    }
    if (node.right != null) {
      queue.enqueue((node.right, level + 1))
      numOfNodesAdded+=1
    }
    numOfNodesAdded
  }


  def bfs(tree: TreeNode, target: Int): Option[TreeNode] = {
    val queue = new mutable.Queue[TreeNode]()
    if (tree.value == target) Some(tree)
    else {
      addChildren(tree, queue)
      while (queue.nonEmpty) {
        val node = queue.dequeue()
        if (node.value == target) return Some(node)
        else {
          addChildren(node, queue)
        }
      }
    }
    None
  }

  def maxLevel(tree: TreeNode): List[(Int, Int)] = {
    val queue = new mutable.Queue[(TreeNode, Int)]()
    val map = new mutable.HashMap[Int, Int]()

    var currentLevel = 1
    var nodesInNextLevel = 0
    nodesInNextLevel +=addChildrenWithLevel(tree, 1, queue)

    while (queue.nonEmpty) {
      val nodeWithLevel = queue.dequeue()
      if(currentLevel!=nodeWithLevel._2){
        map(currentLevel) = nodesInNextLevel
        currentLevel = currentLevel+1
        nodesInNextLevel = 0
      }
      nodesInNextLevel +=addChildrenWithLevel(nodeWithLevel._1, nodeWithLevel._2, queue)
    }
    map.toList
  }


  type Path = List[TreeNode]
  def printPath2Leaf(tree: TreeNode, path: Path):Unit = {
    if(tree.left != null){
      printPath2Leaf(tree.left, path:+tree)
    }
    if(tree.right !=null){
      printPath2Leaf(tree.right, path:+tree)
    }
    if(tree.left == null && tree.right == null){

      printTreeList(path:+tree)
    }

  }

  def printTreeList(path: Path): Unit = {
    val str = new mutable.StringBuilder
    path.foreach(n=>str.append(n.value+", "))
    println(str)
  }


  def rightTravers(tree: TreeNode): Int = {
    var rightValue = 0
    var leftValue = 0
    if(tree.right!=null){
      rightValue=rightTravers(tree.right)
    }
    println(rightValue)
    if(tree.left!=null){
      leftValue = rightTravers(tree.left)
      println(tree.value+rightValue)
    }
    leftValue+tree.value+rightValue
  }
}
