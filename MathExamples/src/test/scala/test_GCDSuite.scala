import GCD._

/*
The MIT License (MIT)
Copyright (c) 2016 wsf.fletcher
*/

import org.scalatest._

class test_GCDSuite extends FunSuite {

  test("GCD Function - small positive integers"){
    assert(GCD.gcd(4,2) === 2, "gcd(4,2) was not 2")
    assert(GCD.gcd(2,4) === 2, "gcd(2,4) was not 2")
    assert(GCD.gcd(10,1) === 1, "gcd(10,1 was not 1")
    assert(GCD.gcd(100,25) === 25, "gcd(100,25) was not 25")
    assert(GCD.gcd(25,100) === 25, "gcd(25,100) was not 25")
    assert(GCD.gcd(121453,1) === 1, "gcd(121453,1) was not 1")

  }


  test("GDC Function - error handling first input negative") {
    intercept[RuntimeException]{
      assert(GCD.gcd(-5, 25) == -1, "gcd error message returned")
    }
  }


  test("GDC Function - error handling on second input negative") {
    intercept[RuntimeException]{
      assert(GCD.gcd(5, -25) == -1, "gcd error message returned")
    }
  }

  test("GDC Function - error handling on both inputs negative") {
    intercept[RuntimeException]{
      assert(GCD.gcd(-5, -25) == -1, "gcd error message returned")
    }
  }

}
