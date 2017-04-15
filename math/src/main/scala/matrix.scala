/**
  * Created by wsf on 4/15/2017.
  */

import scala.collection.mutable.ArrayBuffer

object  matrix {

//  def add(x : Array[Double][Double], y : Array[Double]): ArrayBuffer[Double] = {
//    var z = new ArrayBuffer[Double]
//    for {
//      i <- 0 until x.length
//      j <- 0 until y.length
//    } z += x(i) + y(j)
//
//    z
//  }

  def printVector(a : Array[Double]): Unit = {
    for {
         i <- a.indices
       } println(s"($i) = ${a(i)}")
  }

  def printArrayDim2(a : Array[Array[Double]] ): Unit = {
    for {
      i <- a.indices
      j <- a.indices
    } println(s"($i)($j) = ${a(i)(j)}")

  }


  def main(args: Array[String]): Unit = {
    var x = Array.ofDim[Double](5)
    x(0) = 0.0
    x(1) = 1.0
    x(2) = 2.0
    x(3) = 3.0
    x(4) = 4.0
    printVector(x)

    var y = Array.ofDim[Double](3,3)
    y(0)(0) = 0.0
    y(0)(1) = 1.0
    y(0)(2) = 2.0
    y(1)(0) = -1.0
    y(1)(1) = -1.0
    y(1)(2) = -2.0
    y(2)(0) = 2.0
    y(2)(1) = 3.0
    y(2)(2) = 4.0
    printArrayDim2(y)

  }

}
