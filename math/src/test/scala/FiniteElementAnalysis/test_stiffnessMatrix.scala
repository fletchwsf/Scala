package FiniteElementAnalysis

import org.scalatest.{BeforeAndAfterEach, FunSuite}
import FiniteElementAnalysis.stiffnessMatrix._

import org.scalatest._
import org.scalatest.Matchers._



import scala.collection.mutable.ArrayBuffer

/**
  * Created by wsf on 5/31/2017.
  */
class test_stiffnessMatrix extends FunSuite  {



//  def kBuild( con : Array[Array[Int]],
//              DOF : Int,
  //            localDOF : Int {2,3}
//              eArea : Array[Double],
//              eLength : Array[Double],
//              eModulus : Array[Double]



  test("testKBuild-1 Example 3.3  2 elements and 2 local DOF ") {

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
    var con = Array.ofDim[Int](2,2)
    con(0)(0) = 0
    con(0)(1) = 1
    con(1)(0) = 1
    con(1)(1) = 2

    val result = stiffnessMatrix.kBuild(con, totalDOF, localDOF, eArea, eLength, eModulus)
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


    assert( result === expected, "should return an 2x2 array with all members 1.0 on the diagonals -1.0 elsewhere")
  }

  test("testKBuild-2 3 nodes and 2 local DOF"){

    val eArea = Array(1.0, 1.0)
    val eLength =  Array (1.0, 1.0)
    val eModulus  = Array(1.0, 1.0)

    val nOfNodes = 3 +1
    val nDOFperNode = 1
    val totalDOF = nOfNodes * nDOFperNode
    val localDOF = 2

    var con = Array.ofDim[Int](2,2)
    con(0)(0) = 0
    con(0)(1) = 1
    con(1)(0) = 1
    con(1)(1) = 2

    val example = stiffnessMatrix.kBuild(con, totalDOF, localDOF, eArea, eLength, eModulus)
    var result = Array.ofDim[Double](totalDOF,totalDOF)
    result(0)(0) = 1.0
    result(0)(1) = -1.0
    result(0)(2) = 0.0
    result(1)(0) = -1.0
    result(1)(1) = 2.0
    result(1)(2) = -1.0
    result(2)(0) = 0.0
    result(2)(1) = -1.0
    result(2)(2) = 1.0

    assert(example === result, "should return an 3x3 array with 1,2,1 on the diagonal")
  }

  test("testKBuild-3 2 elements and 3 local DOF"){

    val nElements = 2
    val eArea = Array(1.0, 1.0)
    val eLength =  Array (1.0, 1.0)
    val eModulus  = Array(1.0, 1.0)

    val nNodes = nElements + 1
    val nDOFperNode = 1
    val totalDOF = nNodes * nDOFperNode
    val localDOF = 2

    var con = Array.ofDim[Int](2,2)
    con(0)(0) = 0
    con(0)(1) = 1
    con(1)(0) = 1
    con(1)(1) = 2

    val example = stiffnessMatrix.kBuild(con, totalDOF, localDOF, eArea, eLength, eModulus)
    var result = Array.ofDim[Double](4,4)
    result(0)(0) = 7.0
    result(0)(1) = 1.0
    result(0)(2) = -8.0
    result(0)(3) = 0.0
    result(1)(0) = 1.0
    result(1)(1) = 14.0
    result(1)(2) = -7.0
    result(1)(3) = -8.0
    result(2)(0) = -8.0
    result(2)(1) = -7.0
    result(2)(2) = 23.0
    result(2)(3) = -8.0
    result(3)(0) = 0.0
    result(3)(1) = -8.0
    result(3)(2) = -8.0
    result(3)(3) = 16.0

    assert(example === result, "test with local DOF set to 3 fails")
  }

  test("testKBuild-4 example 3.6 stiffness matrix"){

    val eArea = Array(12.0,9.0)
    val eLength =  Array (45.0, 30.0)
    val eModulus  = Array(200.0,70.0)

    val nOfNodes = 5
    val nDOFperNode = 1
    val totalDOF = nOfNodes * nDOFperNode
    val localDOF = 2

    // note correct the connection matrix entries by -1
    var con = Array.ofDim[Int](2,2)
    con(0)(0) = 3 - 1
    con(0)(1) = 1 - 1
    con(1)(0) = 4 - 1
    con(1)(1) = 2 - 1

    val example = stiffnessMatrix.kBuild(con, totalDOF, localDOF, eArea, eLength, eModulus)
    var result = Array.ofDim[Double](totalDOF,totalDOF)

    result(0)(0) = 53.33
    result(0)(1) = 0.0
    result(0)(2) = -53.33
    result(0)(3) = 0.0
    result(0)(4) = 0.0

    result(1)(0) = 0.0
    result(1)(1) = 21.0
    result(1)(2) = 0.0
    result(1)(3) = -21.0
    result(1)(4) = 0.0

    result(2)(0) = -53.33
    result(2)(1) = 0.0
    result(2)(2) = 53.33
    result(2)(3) = 0.0
    result(2)(4) = 0.0

    result(3)(0) = 0.0
    result(3)(1) = -21.0
    result(3)(2) = 0.0
    result(3)(3) = 21.0
    result(3)(4) = 0.0

    result(4)(0) = 0.0
    result(4)(1) = 0.0
    result(4)(2) = 0.0
    result(4)(3) = 0.0
    result(4)(4) = 0.0

    for(i <- result.indices )
      for(j <- result.indices)
        assert(example(i)(j) === result(i)(j) +- 0.1, "example 3.6 failed")
  }

}
