object Problems {
  def problem1: Tree = {
    def coAlgebra(maxDepth: Int, leftVal: String, rightVal: String): ((Int, String)) => TreeF[(Int, String)] = {
        case (depth, _) if depth == 0 =>
          RootF[(Int, String)](
            (depth + 1, leftVal),
            (depth + 1, rightVal))

        case (depth, value) if depth < maxDepth =>
          NodeF[(Int, String), String](
            (depth + 1, leftVal),
            (depth + 1, rightVal), value)

        case (_, value) => LeafF[(Int, String), String](value)
      }
    val fix = Schemes.ana(coAlgebra(5, "0", "1")).apply((0, ""))
    Tree(fix)
  }

  def problem2: Tree = {
    def coAlgebra(maxDepth:Int, leftVal:String, rightVal:String): ((Int, Int, String)) => TreeF[(Int, Int, String)] = {
        case (depth, _, _) if depth == 0 =>
          RootF[(Int, Int, String)](
            (depth + 1, 0, leftVal),
            (depth + 1, 1, rightVal))

        case (depth, col, value) if depth < maxDepth =>
          NodeF[(Int, Int, String), String](
            (depth + 1, col * 2,  leftVal),
            (depth + 1, col * 2 + 1, rightVal),
            value)

        case (_, col, _) => LeafF[(Int, Int, String), String](
          ('A' + col).toChar.toString)
      }
    val fix = Schemes.ana(coAlgebra(5, "0", "1")).apply((0, 0, ""))
    Tree(fix)
  }

  def main(args: Array[String]): Unit = {
    println(problem1)

    println(problem2)
  }
}
