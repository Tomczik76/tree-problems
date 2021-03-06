trait Tree
case class Root(left: Tree, right: Tree) extends Tree
case class Node[A](left: Tree, right: Tree, value: A) extends Tree
case class Leaf[A](value: A) extends Tree

object Tree {
  def algebra: TreeF[Tree] => Tree = {
    case RootF(left, right) => Root(left, right)
    case NodeF(left, right, value) => Node(left, right, value)
    case LeafF(value) => Leaf(value)
  }
}