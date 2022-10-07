sealed trait Tree

case class Node(value: Int, left: Tree, right: Tree) extends Tree

case object RedLeaf extends Tree

case object YellowLeaf extends Tree

case object GreenLeaf extends Tree

case class treeMethods(tree: Tree) extends Tree {
  def countYellowAndRedValues(tree: Tree): Int = {
    tree match {
      case Node(value, RedLeaf | YellowLeaf, right) =>
        value + countYellowAndRedValues(right)
      case Node(value, left, RedLeaf | YellowLeaf) =>
        value + countYellowAndRedValues(left)
      case Node(_, left, right) => countYellowAndRedValues(left) + countYellowAndRedValues(right)
      case GreenLeaf | RedLeaf | YellowLeaf => 0
    }
  }

  def maxValue(tree: Tree): Option[Int] = {
    tree match {
      case Node(value, left, right) =>
        Some(value max maxValue(left).getOrElse(value) max maxValue(right).getOrElse(value))
      case _ => None
    }
  }
}

object main extends App {
  val tree = Node(30, Node(20, RedLeaf, Node(13, YellowLeaf, YellowLeaf)),
    Node(14, Node(37, RedLeaf, RedLeaf), Node(10, RedLeaf, GreenLeaf))
  )
  println(treeMethods(tree).maxValue(tree))
}
