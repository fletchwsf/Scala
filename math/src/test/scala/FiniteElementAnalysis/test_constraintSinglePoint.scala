package FiniteElementAnalysis

import org.scalatest.Matchers._

import constraintSinglePoint._

import org.scalatest.FunSuite

/**
  * Created by wsf on 6/10/2017.
  */
class test_constraintSinglePoint extends FunSuite {

  // This function finds the maximum value in the testMatrix, multiplies that
  //  value by 10000, and adds that value into the textMatrix at points
  //  define by the constraintArray

  test("add single point constraints at 0,0 and 1,1") {
    val testMatrix = Array.ofDim[Double](2, 2)

    testMatrix(0)(0) = 0.0
    testMatrix(0)(1) = 101.0
    testMatrix(1)(0) = -1.0
    testMatrix(1)(1) = 1.0

    val constraintArray = Array.ofDim[Int](2)
    constraintArray(0) = 0
    constraintArray(1) = 1

    val answerMatrix = Array.ofDim[Double](2, 2)

    answerMatrix(0)(0) = 1010000.0
    answerMatrix(0)(1) = 101.0
    answerMatrix(1)(0) = -1.0
    answerMatrix(1)(1) = 1010001.0

    val result = constraintSinglePoint.add(testMatrix, constraintArray)

    for (i <- result.indices)
      for (j <- result.indices)
        assert(result(i)(j) === answerMatrix(i)(j) +- 0.1, "failed to add single point constraints")
  }

}
