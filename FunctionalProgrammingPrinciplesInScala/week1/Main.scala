package recfun

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
      if(c == 0 || r == c)
        1
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    	def walk(openCounter: Int, closeCounter: Int, balanced: Boolean, chars: List[Char]): Boolean = {
    			if(! balanced)
    			  false
    			else if(chars.isEmpty)
    				openCounter == closeCounter
    			else if(chars.head.compareTo('(') == 0)
    					walk(openCounter + 1, closeCounter, balanced, chars.tail)
    			else if(chars.head.compareTo(')') == 0)
    				if(openCounter < closeCounter + 1)
    					false
    				else
    					walk(openCounter, closeCounter + 1, balanced, chars.tail)
    			else
    				walk(openCounter, closeCounter, balanced, chars.tail)
    	}
    	walk(0, 0, true, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def walk(money: Int, coins: List[Int], counter: Int = 0): Int = {
        if(money < 0)
          counter
        else if(money == 0)
          counter + 1
        else if(coins.isEmpty)
          counter
        else
          walk(money - coins.head, coins, counter) +
          walk(money, coins.tail, counter)
      }
      
      if(money <= 0 || coins.isEmpty)
        0
      else
        walk(money, coins)
    }
  }
