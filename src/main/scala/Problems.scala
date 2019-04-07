object Problems {

  def problem1: Fix[TreeF] = {
    def coAlgebra(totalDepth: Int, leftValue: String, rightValues: String) =
      (tuple: (Int, String)) => tuple match {
        case (depth, _) if depth == 0 =>
          RootF[(Int, String)](
            (depth + 1, leftValue),
            (depth + 1, rightValues))

        case (depth, value) if depth < totalDepth =>
          NodeF[(Int, String), String](
            (depth + 1, leftValue),
            (depth + 1, rightValues), value)

        case (_, value) => LeafF[(Int, String), String](value)
      }
    Schemes.ana(coAlgebra(5, "0", "1")).apply((0, ""))
  }

  def problem2: Fix[TreeF] = {
    def coAlgebra(totalDepth:Int, leftValue:String, rightValues:String) =
      (tuple:(Int, Int, String)) => tuple match {
        case (depth, _, _) if depth == 0 =>
          RootF[(Int, Int, String)](
            (depth + 1, 0, leftValue),
            (depth + 1, 1, rightValues))

        case (depth, col, value) if depth < totalDepth =>
          NodeF[(Int, Int, String), String](
            (depth + 1, col * 2,  leftValue),
            (depth + 1, col * 2 + 1, rightValues),
            value)

        case (_, col, _) => LeafF[(Int, Int, String), String](
          ('A' + col).toChar.toString)
      }
    Schemes.ana(coAlgebra(5, "0", "1")).apply((0, 0, ""))
  }

  def main(args: Array[String]): Unit = {
    println(problem1)

    println(problem2)
  }
}
