/**
  * Created by wsf on 5/7/2017.
  */

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import util.control.Breaks._
import math._
import matrix._

object FEMethods {

  //Kglobal = kBuilder(con, DOF, eArea, eLength, eModulus)


  def kBuilder( con : Array[Array[Integer]],
                constraints : Array[Integer],
                DOF : Integer,
                eArea : Array[Double],
                eLength : Array[Double],
                eModulus : Array[Double]
              ) : Array[Array[Double]] = {
    var  K = Array.ofDim[Double](DOF,DOF)
    var sign = 1

    // build stiffness matrix with no constraints
    for(i <- con.indices) {
      for (j <- 0 to 1 ){
        for ( k <- 0 to 1 ) {
          if (j == k) sign = 1 else sign = -1
          K(con(i)(j))(con(i)(k)) = sign * 1.0 * eModulus(i) * eArea(i) / eLength(i)
        }
      }
    }
    println("original stiffness matrix")
    matrix.prettyPrintDim2(K)

    // add single point constraints
    println("stiffness matrix with single point constraints")
    for(i <- constraints.indices)
      K(constraints(i))(constraints(i)) += 1000.0 * 53
    matrix.prettyPrintDim2(K)



    K
  }


  def findLineFor(lineName: String, fileNameBuffer: ListBuffer[String] ): Int = {
    var lineNum = -1
    for(i <- fileNameBuffer.indices) {
      if (fileNameBuffer(i).contains(lineName)) lineNum = i + 1
    }
    lineNum
  }


  def loadArray(lineName: String, nElements: Int, fileNameBuffer: ListBuffer[String] ): Array[Array[Integer]] = {
    var anArray =  Array.ofDim[Integer](nElements,nElements)
    val startsAt = findLineFor(lineName, fileNameBuffer)
    for ( i <- anArray.indices) {
      val col = fileNameBuffer(i + startsAt).split(",").map(_.trim)
      anArray(i)(0) = col(1).toInt
      anArray(i)(1) = col(2).toInt
    }
    println(s"Integer Array Named:$lineName")
    prettyPrintMatrixInt(anArray)
    anArray
  }
  def loadVector(lineName: String, nElements: Int, fileNameBuffer: ListBuffer[String] ): Array[Double] = {
    var anArray = Array.ofDim[Double](nElements)
    val startsAt = findLineFor(lineName, fileNameBuffer)
    for (i <- anArray.indices) {
      anArray(i) = fileNameBuffer(i + startsAt).toDouble
      println(s"array: $lineName ($i): ${anArray(i)}")
    }
    anArray
  }
  def main(args: Array[String]): Unit = {

    val inputFile = new ListBuffer[String]
    val bufferedSource = Source.fromFile("D:\\Scala\\math\\src\\test\\scala\\FExample_1.txt")
      for (line <- bufferedSource.getLines)
        inputFile += line
    bufferedSource.close

    var DOF_string = new String
    for (i <- inputFile.indices){
      if (inputFile(i).contains("freedom")) DOF_string = inputFile(i+1)
    }
    val DOF : Integer = DOF_string.toInt

    var elementCount_string = new String
    for (i <- inputFile.indices){
      if (inputFile(i).contains("elements")) elementCount_string = inputFile(i+1)
    }
    val elementCount : Integer = elementCount_string.toInt

    var constraints = Array.ofDim[Integer](elementCount)

    val lineN = findLineFor("constraint", inputFile)
    for (i <- constraints.indices)
      constraints(i) = inputFile(i + lineN).toInt - 1

    var connectionTable = Array.ofDim[Integer](elementCount,elementCount)
    connectionTable = loadArray("connection", elementCount, inputFile)

    var eArea = Array.ofDim[Double](elementCount)
    eArea = loadVector("area", elementCount, inputFile)

    var eLength = Array.ofDim[Double](elementCount)
    eLength = loadVector("length", elementCount, inputFile)

    var eModulus = Array.ofDim[Double](elementCount)
    eModulus = loadVector("modulus", elementCount, inputFile)

    var p = Array.ofDim[Double](DOF)
    p = loadVector("force", DOF, inputFile)

    var Kglobal = Array.ofDim[Double](DOF,DOF)

    Kglobal = kBuilder(connectionTable, constraints, DOF, eArea, eLength, eModulus)


    Kglobal(2)(2) += 530000.0
    Kglobal(3)(3) += 530000.0

    Kglobal(1)(1) +=  533300.0
    Kglobal(4)(1) += -444400.0
    Kglobal(1)(4) += -444400.0
    Kglobal(4)(4) +=  370370.37

    Kglobal(0)(0) +=  533300.0
    Kglobal(4)(0) += -177700.0
    Kglobal(0)(4) += -177700.0
    Kglobal(4)(4) +=  59259.260

    val Q = matrix.gaussSeidel(Kglobal, p, 0.000000000001)

  }
}
