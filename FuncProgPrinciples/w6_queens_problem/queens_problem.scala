object QUEENS
{
  def queens(n: Int): Set[List[Int]] = {
    def checks(col: Int, delta: Int, queens: List[Int]): Boolean = queens match {
      case qcol :: others =>
        qcol == col 					//vertical check
        || (qcol - col).abs == delta	//diagonal check
        || checks(col, delta + 1, others)
      case Nil => false
    }

    def isSafe(col: Int, queens: List[Int]): Boolean = !checks(col, 1, queens)

    def placeQueens(k: Int): Set[List[Int]] = {
      if k == 0 then Set(List())
      else
        for
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        yield col :: queens
    }

    placeQueens(n)
  }


  def main(args:Array[String]): Unit = 
  {
    println(queens(8))
  }
}
