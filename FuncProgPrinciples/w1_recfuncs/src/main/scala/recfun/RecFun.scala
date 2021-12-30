package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def fact(num: Int): Int = {
      if (num == 0) 1
      else num * fact(num - 1)
    }
    def binomCoef(k: Int, n: Int): Int = {
      if (n < k) 1
      else fact(n) / (fact(k) * fact(n - k))
    }
    binomCoef(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = { //brackets structure
    // def check(c: List[Char], count: Int): Boolean = c match {
    //   case Nil => count == 0
    //   case ')' :: _ if count < 1 => false
    //   case ')' :: rest => check(rest, count - 1)
    //   case '(' :: rest => check(rest, count + 1)
    //   case _ :: rest => check(rest, count)
    // }
    def check(c: List[Char], count: Int): Boolean = {
      if (c.isEmpty) count == 0
      else if (c.head == ')') {
        if (count < 1) false
        else check(c.tail, count - 1)
      }
      else if (c.head == '(') check(c.tail, count + 1)
      else check(c.tail, count)
    }
    check(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
