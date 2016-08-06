package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balanceLoop(stack: Int, chars: List[Char]): Boolean = {
      if (stack < 0) false
      else if (chars.isEmpty) stack == 0
      else {

        def head: Char = chars.head
        if (head == '(') balanceLoop(stack + 1, chars.tail)
        else if (head == ')') balanceLoop(stack - 1, chars.tail)
        else balanceLoop(stack, chars.tail)
      }
    }

    balanceLoop(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def change(money: Int, coins: List[Int], minCoin: Int): Int = {
      var res: Int = 0
      for (coin <- coins) {
        if (coin >= minCoin) {
          if (coin == money) res += 1
          else if (coin < money) res += change(money - coin, coins, coin)
        }
      }
      res
    }

    change(money, coins, 0)
  }
}
