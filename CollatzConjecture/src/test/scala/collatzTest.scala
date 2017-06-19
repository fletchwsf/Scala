/**
  * Created by wsf on 6/18/2017.
  */


/** A test example based on Scala Test **/



import collatz._

import org.scalatest._

class collatzTest extends org.scalatest.FunSuite {

    test("Collatz conjecture of 7") {
      val answer = Array(7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1)
      assert(collatz.conjecture(7) === answer, "arrays are not matching")
    }

    test ("Collatz conjecture of 35"){
      val answer = Array(35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1)
      assert( collatz.conjecture(35) === answer, "arrays are not matching")
    }

}

