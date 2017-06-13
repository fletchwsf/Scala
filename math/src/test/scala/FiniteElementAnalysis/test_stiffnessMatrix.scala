package FiniteElementAnalysis

/**
  * Created by wsf on 5/31/2017.
  */

import org.scalatest._

import org.scalatest.Matchers._

class test_stiffnessMatrix extends FunSuite  {

  // Check construction of a simple stiffness matrix
  test("testKBuild-2 3 nodes and 2 local DOF"){

    val eArea = Array(1.0, 1.0)
    val eLength =  Array (1.0, 1.0)
    val eModulus  = Array(1.0, 1.0)

    val nOfNodes = 3 +1
    val nDOFperNode = 1
    val totalDOF = nOfNodes * nDOFperNode
    val localDOF = 2

    var con = Array.ofDim[Integer](2,2)
    con(0)(0) = 0
    con(0)(1) = 1
    con(1)(0) = 1
    con(1)(1) = 2

    val result = stiffnessMatrix.kBuild(con, totalDOF, localDOF, eArea, eLength, eModulus)
    var answer = Array.ofDim[Double](totalDOF,totalDOF)
    answer(0)(0) = 1.0
    answer(0)(1) = -1.0
    answer(0)(2) = 0.0
    answer(1)(0) = -1.0
    answer(1)(1) = 2.0
    answer(1)(2) = -1.0
    answer(2)(0) = 0.0
    answer(2)(1) = -1.0
    answer(2)(2) = 1.0

    assert(result === answer, "should return an 3x3 array with 1,2,1 on the diagonal")
  }

  // Test case is based on example 3.3 in Chandrupatal (C)2002
  test("testKBuild-1 Example 3.3 with 2 elements and local DOF set to 2") {

    val nElements = 2
    val eArea = Array(5.25, 3.75)
    val eLength =  Array (1.0, 1.0)
    val eModulus  = Array(1.0, 1.0)

    assert(eArea.length === nElements, "check element count consistencey")
    assert(eLength.length === nElements, "check element count consistencey")
    assert(eModulus.length === nElements, "check element count consistencey")

    val nNodes = nElements + 1
    val nDOFPerNode = 1
    val totalDOF = nNodes * nDOFPerNode
    val localDOF = 2
    var con = Array.ofDim[Integer](2,2)
    con(0)(0) = 0
    con(0)(1) = 1
    con(1)(0) = 1
    con(1)(1) = 2

    val answer = stiffnessMatrix.kBuild(con, totalDOF, localDOF, eArea, eLength, eModulus)
    var expected = Array.ofDim[Double](totalDOF,totalDOF)
    expected(0)(0) = 5.25
    expected(0)(1) = -5.25
    expected(0)(2) = 0.0
    expected(1)(0) = -5.25
    expected(1)(1) = 9.0
    expected(1)(2) = -3.75
    expected(2)(0) = 0.0
    expected(2)(1) = -3.75
    expected(2)(2) = 3.75

    assert( answer === expected, "should return an 2x2 array with all members 1.0 on the diagonals -1.0 elsewhere")
  }

  // Test case is based on example 3.6 in Chandrupatal (C)2002
  test("testKBuild-3 example 3.6 stiffness matrix"){

    val eArea = Array(12.0,9.0)
    val eLength =  Array (45.0, 30.0)
    val eModulus  = Array(200.0,70.0)

    val nOfNodes = 5
    val nDOFperNode = 1
    val totalDOF = nOfNodes * nDOFperNode
    val localDOF = 2

    // note correct the connection matrix entries by -1
    var con = Array.ofDim[Integer](2,2)
    con(0)(0) = 3 - 1
    con(0)(1) = 1 - 1
    con(1)(0) = 4 - 1
    con(1)(1) = 2 - 1

    val result = stiffnessMatrix.kBuild(con, totalDOF, localDOF, eArea, eLength, eModulus)
    var answer = Array.ofDim[Double](totalDOF,totalDOF)

    answer(0)(0) = 53.33
    answer(0)(1) = 0.0
    answer(0)(2) = -53.33
    answer(0)(3) = 0.0
    answer(0)(4) = 0.0

    answer(1)(0) = 0.0
    answer(1)(1) = 21.0
    answer(1)(2) = 0.0
    answer(1)(3) = -21.0
    answer(1)(4) = 0.0

    answer(2)(0) = -53.33
    answer(2)(1) = 0.0
    answer(2)(2) = 53.33
    answer(2)(3) = 0.0
    answer(2)(4) = 0.0

    answer(3)(0) = 0.0
    answer(3)(1) = -21.0
    answer(3)(2) = 0.0
    answer(3)(3) = 21.0
    answer(3)(4) = 0.0

    answer(4)(0) = 0.0
    answer(4)(1) = 0.0
    answer(4)(2) = 0.0
    answer(4)(3) = 0.0
    answer(4)(4) = 0.0

    for(i <- answer.indices )
      for(j <- answer.indices)
        assert(result(i)(j) === answer(i)(j) +- 0.1, "example 3.6 failed")
  }

  // Test case is based on example 3.7 in Chandrupatal (C)2002
  test("testKBuild-4 Check stiffness matrix construction for 2 elements have local DOF set to 3"){

    val nElements = 2
    val eArea = Array(1.0, 1.0)
    val eLength =  Array (1.0, 1.0)
    val eModulus  = Array(1.0, 1.0)

    val nNodes = 5
    val nDOFperNode = 1
    val totalDOF = nNodes * nDOFperNode
    val localDOF = 3

    // note - correct the connection array by -1
    //  1 | 1, 3, 2
    //  2 | 3, 5, 4
    var con = Array.ofDim[Integer](2,3)
    con(0)(0) = 1 - 1
    con(0)(1) = 3 - 1
    con(0)(2) = 2 - 1
    con(1)(0) = 3 - 1
    con(1)(1) = 5 - 1
    con(1)(2) = 4 - 1

    val result = stiffnessMatrix.kBuild(con, totalDOF, localDOF, eArea, eLength, eModulus)
    var answer = Array.ofDim[Double](5,5)
    answer(0)(0) = 7.0
    answer(0)(1) = -8.0
    answer(0)(2) = 1.0
    answer(0)(3) = 0.0
    answer(0)(4) = 0.0
    answer(1)(0) = -8.0
    answer(1)(1) = 16.0
    answer(1)(2) = -8.0
    answer(1)(3) = 0.0
    answer(1)(4) = 0.0
    answer(2)(0) = 1.0
    answer(2)(1) = -8.0
    answer(2)(2) = 14.0
    answer(2)(3) = -8.0
    answer(2)(4) = 1.0
    answer(3)(0) = 0.0
    answer(3)(1) = 0.0
    answer(3)(2) = -8.0
    answer(3)(3) = 16.0
    answer(3)(4) = -8.0
    answer(4)(0) = 0.0
    answer(4)(1) = 0.0
    answer(4)(2) = 1.0
    answer(4)(3) = -8.0
    answer(4)(4) = 7.0

    assert(result === answer, "test with local DOF set to 3 fails")
  }


}
