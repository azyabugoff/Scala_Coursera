//FROM geeksforgeeks.org
// Scala program of GCD using recursion
import scala.annotation.tailrec

// Creating object
object GFG
{
	// Function defined
	def GCD(n: Int, m: Int): Int =
	{
		// tail recursion function defined
		@tailrec def gcd(x:Int, y:Int): Int=
		{
			if (y == 0) x
			else gcd(y, x % y)
		}
		gcd(n, m)
	}
	
	// Main method
	def main(args:Array[String]): Unit = 
    {
        println(GCD(12, 18))
    }
}
