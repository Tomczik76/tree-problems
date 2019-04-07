trait TreeF[A]
case class RootF[A](left: A, right: A) extends TreeF[A]
case class NodeF[A, B](left: A, right: A, value: B) extends TreeF[A]
case class LeafF[A, B](value: B) extends TreeF[A]

object TreeF {
  implicit val functorInstance: Functor[TreeF] = new Functor[TreeF] {
    override def map[A, B](fa: TreeF[A])(fn: A => B): TreeF[B] = fa match {
      case RootF(left, right) => RootF(fn(left), fn(right))
      case NodeF(left, right, value) => NodeF(fn(left), fn(right), value)
      case LeafF(value) => LeafF(value)
    }
  }
}