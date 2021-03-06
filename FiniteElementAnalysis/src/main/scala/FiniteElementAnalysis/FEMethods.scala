package FiniteElementAnalysis

/**
  * Created by wsf on 5/7/2017.
  */

import scala.collection.mutable.ListBuffer
import scala.io.Source

import calculateStress._
import matrix._

object FEMethods {

  def kBuild( con : Array[Array[Int]],
                DOF : Int,
                eArea : Array[Double],
                eLength : Array[Double],
                eModulus : Array[Double]
              ) : Array[Array[Double]] = {
    val K = Array.ofDim[Double](DOF, DOF)
    var sign = 1

    // build stiffness matrix with no constraints
    //     note: this code includes the matrix [1,-1,-1,1] implicitly in the if statement
    for (i <- con.indices) {
      for (j <- 0 to 1) {
        for (k <- 0 to 1) {
          if (j == k) sign = 1 else sign = -1
          K(con(i)(j))(con(i)(k)) = sign * 1.0 * eModulus(i) * eArea(i) / eLength(i)
        }
      }
    }
    K
  }

  def addSinglePointConstraints(
                                 kMatrix : Array[Array[Double]],
                                  constraints : Array[Int],
                                  C: Double): Array[Array[Double]] = {

    for(i <- constraints.indices)
      kMatrix(constraints(i))(constraints(i)) += C
    kMatrix
  }

  def addMultiPointConstraints(  K2 : Array[Array[Double]],
                                 C: Double,
                                 nodesArray: Array[Array[Int]],
                                 qRatios: Array[Double]
                              ): Array[Array[Double]] = {

    val B = Array.ofDim[Double](3)
    B(0) = 0.0

    val kmOne = Array.ofDim[Double](2, 2)

    for (i <- nodesArray.indices) {
      B(1) = 1.0
      B(2) = qRatios(i)
      kmOne(0)(0) = C*B(1)*B(1)
      kmOne(0)(1) = C*B(1)*B(2)
      kmOne(1)(0) = C*B(2)*B(1)
      kmOne(1)(1) = C*B(2)*B(2)

      for (j <- nodesArray.indices)
        for (k <- nodesArray.indices) {
          //K2(nodesArray(i)(j))(nodesArray(i)(k)) += kmOne(i)(j)
          println(s"K2 numbering: ${nodesArray(i)(j)}${nodesArray(i)(k)} += ${kmOne(j)(k)}")
          K2(nodesArray(i)(j))(nodesArray(i)(k)) += kmOne(j)(k)
        }
    }
    K2
  }

  def findLineFor(lineName: String, fileNameBuffer: ListBuffer[String] ): Int = {
    var lineNum = -1
    for(i <- fileNameBuffer.indices) {
      if (fileNameBuffer(i).contains(lineName)) lineNum = i + 1
    }
    lineNum
  }
  def loadArray(lineName: String, nElements: Int, fileNameBuffer: ListBuffer[String] ): Array[Array[Int]] = {
    val anArray = Array.ofDim[Int](nElements, nElements)
    val startsAt = findLineFor(lineName, fileNameBuffer)
    for ( i <- anArray.indices) {
      val col = fileNameBuffer(i + startsAt).split(",").map(_.trim)
      anArray(i)(0) = col(1).toInt - 1
      anArray(i)(1) = col(2).toInt - 1
    }
    println(s"loading array:$lineName")
    matrix.printArray(anArray)
    anArray
  }
  def loadVector(lineName: String, nElements: Int, fileNameBuffer: ListBuffer[String] ): Array[Double] = {
    val anArray = Array.ofDim[Double](nElements)
    val startsAt = findLineFor(lineName, fileNameBuffer)
    println(s" loading array: $lineName")
    for (i <- anArray.indices) {
      anArray(i) = fileNameBuffer(i + startsAt).toDouble
      println(s" | ${anArray(i)} |")
    }
    anArray
  }

  def solverOneDOF(inputFileName:String) : Array[Double] = {

    // read the input file into memory
    val inputFile = new ListBuffer[String]
    val bufferedSource = Source.fromFile(inputFileName)
    for (line <- bufferedSource.getLines)
      inputFile += line
    bufferedSource.close

    // read the Degree-of-Freedom DOF value for the model
    var DOF_string = new String
    for (i <- inputFile.indices){
      if (inputFile(i).contains("freedom")) DOF_string = inputFile(i+1)
    }
    val DOF : Int = DOF_string.toInt

    // read in the  number of elements in the model
    var elementCount_string = new String
    for (i <- inputFile.indices){
      if (inputFile(i).contains("elements")) elementCount_string = inputFile(i+1)
    }
    val elementCount : Int = elementCount_string.toInt

    // read in the nodes with single point constraints
    val constraints = Array.ofDim[Int](elementCount)
    val lineN = findLineFor("constraint", inputFile)
    for (i <- constraints.indices)
      constraints(i) = inputFile(i + lineN).toInt - 1

    // Setup the element connection table array
    var connectionTable = Array.ofDim[Int](elementCount,elementCount)
    connectionTable = loadArray("connection", elementCount, inputFile)

    // Setup the element cross-sectional area array
    var eArea = Array.ofDim[Double](elementCount)
    eArea = loadVector("area", elementCount, inputFile)

    // Setup the element length array
    var eLength = Array.ofDim[Double](elementCount)
    eLength = loadVector("length", elementCount, inputFile)

    // Setup the element modulus of elasticity array
    var eModulus = Array.ofDim[Double](elementCount)
    eModulus = loadVector("modulus", elementCount, inputFile)

    // Setup the element body force array
    var p = Array.ofDim[Double](DOF)
    p = loadVector("force", DOF, inputFile)

    // Setup the multi-point constraint array
    //    number of points - NMP
    // read in the  number of elements in the model
    var nMultiPointsCount_string = new String
    for (i <- inputFile.indices){
      if (inputFile(i).contains("NMP")) nMultiPointsCount_string = inputFile(i+1)
    }
    val NMP : Int = nMultiPointsCount_string.toInt
    println(s"number of multipoint nodes:$NMP")
    //  read in the multi-point constraints
    val mpNodes = Array.ofDim[Int](NMP, 2)
    val mpQRatio = Array.ofDim[Double](NMP)
    val startsAt = findLineFor("multipoint", inputFile)
    for ( i <- mpNodes.indices) {
      val col = inputFile(i + startsAt).split(",").map(_.trim)
      mpNodes(i)(0) = col(0).toInt - 1
      mpNodes(i)(1) = col(1).toInt - 1
      mpQRatio(i) = col(2).toDouble
    }
    println("Multipoint displacement vector")
    printVector(mpQRatio)
    println("Multipoint element numbers")
    matrix.printArray(mpNodes)


    println("---------------------------------------------------")

    // Build the initial stiffness stiffness matrix
    var Kg = Array.ofDim[Double](DOF,DOF)
    Kg = kBuild(connectionTable, DOF, eArea, eLength, eModulus)
    println("initialized stiffness array")
    matrix.printArray(Kg)

    // Establish the penalty stiffness based on the maximum stiffness in the matrix
    val C : Double  = matrix.max(Kg) * 10000.0
    println(s"maximum stiffness value:$C for single point constraints")
    println("stiffness matrix with single point constraints \n")

    // Add the single point constraint values to the stiffness matrix
    Kg = addSinglePointConstraints(Kg, constraints, C)
    println("with single point constraints")
    matrix.printArray(Kg)

    // check for multipoint constraints and
    //    add the multipoint constraint values to the stiffness matrix
    if ( NMP > 0 ) {
      Kg = addMultiPointConstraints(Kg, C, mpNodes, mpQRatio)
      println("with multi-point constraints")
      matrix.printArray(Kg)
    } else Kg

    // solve for the displacement vector
    val Q2 = matrix.gaussSeidel(Kg, p, 0.0000000000001)

    // solve for stress
    println("element stress")
    println("expected: | 21.6 | 28.35 |")
    val eStress = calculateStress.stress(connectionTable, Q2, eLength, eModulus)
    //matrix.printVector(eStress.map( _ * 1000.0))
    matrix.printVector(eStress)
    println("-------------------------------------------------------")
    // Return the displacement vector
    Q2



  }
  def main(args: Array[String]): Unit = {

   var inputFileName = "D:\\Scala\\FiniteElementAnalysis\\src\\test\\scala\\test_FiniteElementAnalysis\\FExample_3-6.txt"

    // solve for element displacement
   // var Q : Array[Double] = solverOneDOF(args(1))
   var Q : Array[Double] = solverOneDOF(inputFileName)
    println(s"Running file named:$args(1)")
    println("displacement array - Q")
    matrix.printVector(Q)

    println("need to get expected results from input file heading")
    println("missing function to calculate stresses")





  }
}
