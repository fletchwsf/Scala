/**
  * Created by wsf on 6/18/2017.
  */


// A program to check the Collatz Conjecture for a positive integer
//
// The Collatz conjecture states that for any positive integer n the
//   the following algorithm will reduce n to 1.
//   for f(n), if n is even then divide n / 2 and call f(n_new), if n is odd
//   then calculate n * 3 + 1 and call f(n_new)

import scala.collection.mutable.ArrayBuffer

//-----------------------------------------------------------------------------
// conjecture calculates the Collatz conjecture for any positive integer n.
//   and returns the reduction sequence
//-----------------------------------------------------------------------------


object collatz {

    def conjecture(n: Long): ArrayBuffer[Long] = {
      val buffer = new ArrayBuffer[Long] ()           // a buffer to hold the result

      def loop(x: Long): Long = {                   // x will be reduced to 1
        buffer += x
        if( x <= 1)  x
        else if(isEven(x)) loop( x / 2)          // divide even numbers by 2
        else loop( x * 3 + 1 )                   // odd numbers are n * 3 + 1
      }

      def isEven(n: Long): Boolean =
        if (n % 2 == 0 ) true
        else false

      loop(n)
      // returns the buffer
      buffer
    }

    def many(n: Long): ArrayBuffer[Long] = {
      var buffer = new ArrayBuffer[Long]()
      if (n >= 2 ) {
        buffer = conjecture(n)
        println("f(n)=" + buffer)
      }
      if (n >= 2) many(n-1)
      buffer
    }


    // def calcRange(start: Long, end: Long): ArrayBuffer[Long]= {
    //     var buffer = new ArrayBuffer[Long]()
    //      @annotation.tailrec
    //      def loop(n: Long): ArrayBuffer[Long] = {
    //       if ( n <= end ) {
    //       conjecture(n)
    //       loop(n + 1)
    //      }
    //   }
    //   loop(start)
    //   buffer
    // }



    def main(args: Array[String]): Unit = {
      println(s"Calculating Collatz Conjecture for:$args(1)")
      println(conjecture(args(0).toLong))
    }
}
