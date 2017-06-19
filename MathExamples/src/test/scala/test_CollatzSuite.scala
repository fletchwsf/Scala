import Collatz._

/* Comments, test cases, and other code changes are
The MIT License (MIT)
Copyright (c) 2016 wsf.fletcher
*/

import org.scalatest._

class test_CollatzSuite extends FunSuite{


  test("Collatz conjecture of 7") {
    val answer = Array(7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1)
    assert(Collatz.conjecture(7) === answer, "arrays are not matching")
  }

  test ("Collatz conjecture of 35"){
    val answer = Array(35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1)
    assert( Collatz.conjecture(35) === answer, "arrays are not matching")
  }


}
