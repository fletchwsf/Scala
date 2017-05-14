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

  def loadData(lineName: String, nElements: Int, fileNameBuffer: ListBuffer[String] ): Array[Double] = {
    var anArray = Array.ofDim[Double](nElements)
    val startsAt = findLineFor(lineName, fileNameBuffer)
    for (i <- anArray.indices) {
      anArray(i) = fileNameBuffer(i + startsAt).toDouble
      println(s"array: $lineName ($i): ${anArray(i)}")
    }
    anArray
  }
  def main(args: Array[String]): Unit = {

//    val bufferedSource = Source.fromFile("D:\\Scala\\math\\src\\test\\scala\\FExample_1.txt")
//    var textRow = new String()
//    var numberRow = new Array[String](30)
//    for (line <- bufferedSource.getLines) {
//      //println(line.toUpperCase)
//      // if line starts with # line is string else it is number
//      //    or list of numbers
//      if (line.contains("#")) {
//        println(s"$line")
//        textRow = line
//      }
//      else {
//        val col = line.split(",").map(_.trim)
//        for (e <- col)
//          println(s"|$e|")
//          numberRow = col
//      }
//
//      //println(s"${cols(0)}|")
//    }
//    bufferedSource.close

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

    val constraints = Array.ofDim[Integer](elementCount)
      constraints(0) = 2
      constraints(1) = 3

    val con = Array.ofDim[Integer](elementCount,elementCount)
    con(0)(0) = 2
    con(0)(1) = 0
    con(1)(0) = 3
    con(1)(1) = 1

    var eArea = Array.ofDim[Double](elementCount)
    eArea = loadData("area", elementCount, inputFile)

    var eLength = Array.ofDim[Double](elementCount)
    eLength = loadData("length", elementCount, inputFile)

    var eModulus = Array.ofDim[Double](elementCount)
    eModulus = loadData("modulus", elementCount, inputFile)

    var Kglobal = Array.ofDim[Double](DOF,DOF)

    Kglobal = kBuilder(con, constraints, DOF, eArea, eLength, eModulus)


//    Kglobal(2)(2) += 530000.0
//    Kglobal(3)(3) += 530000.0
//
//    Kglobal(1)(1) +=  533300.0
//    Kglobal(4)(1) += -444400.0
//    Kglobal(1)(4) += -444400.0
//    Kglobal(4)(4) +=  370370.37
//
//    Kglobal(0)(0) +=  533300.0
//    Kglobal(4)(0) += -177700.0
//    Kglobal(0)(4) += -177700.0
//    Kglobal(4)(4) +=  59259.260

    val p = Array.ofDim[Double](DOF)
    for (i <- p.indices) p(i) = 0.0
    p(4) = 30.0


    val Q = matrix.gaussSeidel(Kglobal, p, 0.000000000001)

  }
}
