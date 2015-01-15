package recfun
import common._

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
    if(c==0 || c==r)
      1
    else{
      pascal(c-1, r-1) + pascal (c, r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], b: Int) : Boolean = {
      if(chars.isEmpty)
        b == 0
      else{
        if(chars.head == '(')
          balance(chars.tail, b+1)
        else if(chars.head == ')'){
          if(b>0)
            balance(chars.tail, b-1)
          else
            false
        }
        else balance(chars.tail, b)
      }
    }
    
    balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0)
        0
      0
  }
}