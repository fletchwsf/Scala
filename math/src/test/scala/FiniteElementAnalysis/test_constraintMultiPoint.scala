package FiniteElementAnalysis

import FiniteElementAnalysis.FEMethods.addMultiPointConstraints
import org.scalatest.FunSuite
import org.scalatest.Matchers._

/**
  * Created by wsf on 6/10/2017.
  */
class test_constraintMultiPoint extends FunSuite {


  //  def add(  K2 : Array[Array[Double]],
  //            C: Double,
  //            nodesArray: Array[Array[Integer]],
  //            qRatios: Array[Double]
  //         ): Array[Array[Double]] = {


  // Test case is based on example 3.6 in Chandrupatal (C)2002
  test("testKBuild-3 example 3.6 stiffness matrix") {

    val eArea = Array(12.0, 9.0)
    val eLength = Array(45.0, 30.0)
    val eModulus = Array(200.0, 70.0)

    val nOfNodes = 5
    val nDOFperNode = 1
    val totalDOF = nOfNodes * nDOFperNode
    val localDOF = 2

    // note correct the connection matrix entries by -1
    var con = Array.ofDim[Integer](2, 2)
    con(0)(0) = 3 - 1
    con(0)(1) = 1 - 1
    con(1)(0) = 4 - 1
    con(1)(1) = 2 - 1

    // multi point nodes with node number correction of -1
    var mpNodes = Array.ofDim[Integer](2,2)
    mpNodes(0)(0) = 1 - 1
    mpNodes(0)(1) = 5 - 1
    mpNodes(1)(0) = 2 - 1
    mpNodes(1)(1) = 5 - 1

    var mpQRatio = Array.ofDim[Double](2)
    mpQRatio(0) = -0.333
    mpQRatio(1) = -0.833

    val tmp1 = stiffnessMatrix.kBuild(con, totalDOF, localDOF, eArea, eLength, eModulus)

    // Establish the penalty stiffness based on the maximum stiffness in the matrix
    val C : Double  = matrix.max(tmp1) * 10000.0

    // Add single point constraints
    val spNodes = Array.ofDim[Integer](2)
    spNodes(0) = 3 - 1
    spNodes(1) = 4 - 1

    val tmp2 = constraintSinglePoint.add(tmp1, spNodes)

    //addMultiPointConstraints(Kg, C, mpNodes, mpQRatio)
    val result = constraintMultiPoint.add(tmp2, C, mpNodes, mpQRatio)

    var answer = Array.ofDim[Double](totalDOF, totalDOF)

    answer(0)(0) = 533386.7
    answer(0)(1) = 0.0
    answer(0)(2) = -53.33
    answer(0)(3) = 0.0
    answer(0)(4) = -177600.0

    answer(1)(0) = 0.0
    answer(1)(1) = 533354.3
    answer(1)(2) = 0.0
    answer(1)(3) = -21.0
    answer(1)(4) = -444266.667

    answer(2)(0) = -53.33
    answer(2)(1) = 0.0
    answer(2)(2) = 533386.7
    answer(2)(3) = 0.0
    answer(2)(4) = 0.0

    answer(3)(0) = 0.0
    answer(3)(1) = -21.0
    answer(3)(2) = 0.0
    answer(3)(3) = 533354.3
    answer(3)(4) = 0.0

    answer(4)(0) = -177600.0
    answer(4)(1) = -444266.667
    answer(4)(2) = 0.0
    answer(4)(3) = 0.0
    answer(4)(4) = 429214.933

    for (i <- answer.indices)
      for (j <- answer.indices)
        assert(result(i)(j) === answer(i)(j) +- 10.0, "example 3.6 failed")

    var p = Array.ofDim[Double](5)
    p(0) = 0.0
    p(1) = 0.0
    p(2) = 0.0
    p(3) = 0.0
    p(4) = 30.0


    // solve for the displacement vector
    val Q2 = matrix.gaussSeidel(answer, p, 0.0000000000001)

    // solve for stress
    println("element stress")
    println("expected: | 21.6 | 28.35 |")
    val eStress = calculateStress.stress(con, Q2, eLength, eModulus)
    matrix.printVector(eStress.map( _ * 1000.0))


  }
}
