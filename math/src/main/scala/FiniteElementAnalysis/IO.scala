package FiniteElementAnalysis

import scala.collection.mutable.ListBuffer

/**
  * Created by wsf on 5/29/2017.
  */

object IO {

  def findLineFor(lineName: String, fileNameBuffer: ListBuffer[String] ): Int = {
    var lineNum = -1
    for(i <- fileNameBuffer.indices) {
      if (fileNameBuffer(i).contains(lineName)) lineNum = i + 1
    }
    lineNum
  }

  def readInts(lineName: String, fileName: ListBuffer[String]): Array[Int] = {
    var outList =  Array[Int]()
    val startReadAt = findLineFor(lineName, fileName) + 1
    // expecting a row of integers separated by commas
    val rowInts = fileName(startReadAt).split(",").map(_.trim)
    outList = rowInts.map(_.toInt)

    println(s"loading array of integers:")
    matrix.printVector(outList)
    outList
  }




  def loadArray(lineName: String, nElements: Int, fileNameBuffer: ListBuffer[String] ): Array[Array[Integer]] = {
    var anArray =  Array.ofDim[Integer](nElements,nElements)
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
    var anArray = Array.ofDim[Double](nElements)
    val startsAt = findLineFor(lineName, fileNameBuffer)
    println(s" loading array: $lineName")
    for (i <- anArray.indices) {
      anArray(i) = fileNameBuffer(i + startsAt).toDouble
      println(s" | ${anArray(i)} |")
    }
    anArray
  }

}
