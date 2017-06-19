// A program to check the Collatiz Conjecture for a positive integer
//
// The Collatiz conjecture states that for any positive integer n the
//   the following algorithm will reduce n to 1.
//   for f(n), if n is even then divide n / 2 and call f(n_new), if n is odd
//   then calculate n * 3 + 1 and call f(n_new)

import scala.collection.mutable.ArrayBuffer

//-----------------------------------------------------------------------------
// conjecture calculates the Collatiz conjecture for any positive integer n.
//   and returns the reduction sequence
//-----------------------------------------------------------------------------

object Collatz  {

  def conjecture(n: Int): ArrayBuffer[Int] = {
    val buffer = new ArrayBuffer[Int] ()           // a buffer to hold the result

    def loop(x: Int): Int = {                   // x will be reduced to 1
      buffer += x
      if( x <= 1)  x
      else if(isEven(x)) loop( x / 2)          // divide even numbers by 2
      else loop( x * 3 + 1 )                   // odd numbers are n * 3 + 1
    }

    def isEven(n: Int): Boolean =
      if (n % 2 == 0 ) true
      else false

    loop(n)
    // returns the buffer
    buffer
  }

  def many(n: Int): ArrayBuffer[Int] = {
    var buffer = new ArrayBuffer[Int]()
    if (n >= 2 ) {
      buffer = conjecture(n)
      println("f(n)=" + buffer)
    }
    if (n >= 2) many(n-1)
    buffer
  }


  // def calcRange(start: Int, end: Int): ArrayBuffer[Int]= {
  //     var buffer = new ArrayBuffer[Int]()
  //      @annotation.tailrec
  //      def loop(n: Int): ArrayBuffer[Int] = {
  //       if ( n <= end ) {
  //       conjecture(n)
  //       loop(n + 1)
  //      }
  //   }
  //   loop(start)
  //   buffer
  // }



  def main(args: Array[String]): Unit = {
    println(conjecture(48))
    conjecture(48)
    many(100)
  }

}
