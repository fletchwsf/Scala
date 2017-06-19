package FiniteElementAnalysis

/**
  * Created by wsf on 4/15/2017.
  */

import scala.math._
import scala.util.control.Breaks._

object  matrix {

  def addVectors(x: Array[Double], y : Array[Double]) : Array[Double] = {
    val z = Array.ofDim[Double](x.length)
    for {
      i <- x.indices
    } z(i) = x(i) + y(i)
    z
  }

  def subVectors(x: Array[Double], y : Array[Double]) : Array[Double] = {
    val z = Array.ofDim[Double](x.length)
    for {
      i <- x.indices
    } z(i) = x(i) - y(i)
    z
  }

  def add(x : Array[Array[Double]], y : Array[Array[Double]]): Array[Array[Double]] = {
    val z = Array.ofDim[Double](x.length, y.length)
    for {
      i <- x.indices
      j <- x.indices
    } z(i)(j) = x(i)(j) + y(i)(j)
    z
  }

  def sumOf(x : Array[Double]): Double = {
    var acc : Double = 0.0
    for(i <- x.indices) acc += x(i)
    acc
  }

  def subtract(x : Array[Array[Double]], y : Array[Array[Double]]): Array[Array[Double]] = {
    val z = Array.ofDim[Double](x.length, y.length)
    for {
      i <- x.indices
      j <- x.indices
    } z(i)(j) = x(i)(j) - y(i)(j)
    z
  }

  def multiply(x : Array[Array[Double]], y : Array[Array[Double]]): Array[Array[Double]] = {
    val z = Array.ofDim[Double](x.length, y.length)
    for {
      i <- x.indices
      j <- x.indices
      k <- y.indices
    } z(i)(j) += x(i)(j) * y(k)(j)
    z
  }

  def max(x: Array[Array[Double]]) : Double = {
    var maxValue = 0.0
    for {
      i <- x.indices
      j <- x.indices
    } if ( maxValue < x(i)(j) ) maxValue = x(i)(j)
    maxValue
  }
  def gaussianElimination(a : Array[Array[Double]], d : Array[Double]): Array[Double] = {
    println("----------------------------------------")
    println("Gassuian Elimination z,d")
    println("----------------------------------------")
    prettyPrintAxB(a,d)
    val x = Array.ofDim[Double](a.length)
    val n = a.length -1
    for ( i <- 0 until n ){
      for (k <- i + 1 to n){
        val m = a(k)(i) / a(i)(i)
        a(k)(i) = 0
        for (j <- i + 1 to n) {
          a(k)(j) -= m * a(i)(j)
        }
        d(k) -= m * d(i)
      }
    }
    // back substitution
    for (ii <- a.indices ) {
      val i = n - ii
      for ( j <- i + 1 to n){
        d(i) -= a(i)(j) * x(j)
      }
      x(i) = d(i)/ a(i)(i)
    }
    println("solution x = ")
    printVector(x)
    x
  }

  def conjugateGradientSolver(a : Array[Array[Double]], b : Array[Double], epsilon: Double): Array[Double] = {
    println("----------------------------------------")
    println("Conjugate Gradient Solver")
    println("----------------------------------------")

    var convergence = false
    // temporary values
    var gg1 : Double = 0.0
    // define arrays
    val ad = Array.ofDim[Double](b.length)
    val x = Array.ofDim[Double](b.length)
    val g = Array.ofDim[Double](b.length)
    val d = Array.ofDim[Double](b.length)
    // initialize the arrays
    for ( i <- x.indices){
      x(i) = 0.0
      g(i) = -b(i)
      d(i) = b(i)
    }
    for ( i <- g.indices) gg1 += g(i) * g(i)

    // loop until the error term is less than epsilon
    //var itr : Int = 0
    //var diff = 1.0

      for (itr <- 0 to 20) {
        var dad = 0.0
        for (i <- d.indices) {
          var c: Double = 0.0
          for (j <- d.indices) {
            c += a(i)(j) * d(j)
          }
          ad(i) = c
          dad += c * d(i)
        }
        val al = gg1 / dad
        var gg2 = 0.0
        for (i <- d.indices) {
          x(i) += al * d(i)
          g(i) += al * ad(i)
          gg2 += g(i) * g(i)
        }
        val bt = gg2 / gg1
        for (i <- d.indices) d(i) = -g(i) + bt * d(i)
        //for (i <- d.indices) d(i) = -g(i) + gg2 * d(i) / gg1
        gg1 = gg2

        if (gg1 < epsilon) {
          println(s"solver used $itr iterations")
          convergence = true
          //itr = 501
          break
        }
        //itr += 1
       // println(s"iteration:$itr, gg1:$gg1")
      }

    println("Solution:")
    println(s"Convergence:$convergence")
    printVector(x)
    x
  }

  def gaussSeidel(a : Array[Array[Double]], d : Array[Double], epsilon: Double): Array[Double] = {
    println("----------------------------------------")
    println("Gauss-Seidel Solver Ax = b ")
    println("----------------------------------------")
    prettyPrintAxB(a,d)
    val x = Array.ofDim[Double](d.length)
    for (j <- x.indices) x(j) = 0.0
    val y = Array.ofDim[Double](a.length)
    for (i <- y.indices) y(i) = 0.0
    var diff = 0.0
    val n = a.length - 1
    var stop = false
    //var ctr = 0
    breakable {
      for (ctr <- 1 to 100000) {
        diff = 0.0
        for (k <- 0 to n) {
          var sum1, sum2 = 0.0
          // first inner sum term
          for (j <- 0 to k) {
            sum1 += a(k)(j) * y(j)
           // println(s"sum1: a($k)($j) * y($j): $sum1")
          }
          // second inner sum term
          for (j <- k + 1 to n) {
            sum2 += a(k)(j) * x(j)
           // println(s"sum2: a($k)($j) * x($j): $sum2")
          }
          // current estimate
          y(k) = (d(k) - sum1 - sum2 ) / a(k)(k)
          //println(s"y($k): ${y(k)} = d($k) - $sum1 - $sum2 / a($k)($k) ")

          // update sum of squares difference
          diff = diff + pow(x(k) - y(k),2)
        }
        //println(s"diff:$diff")

        // set x == y, and clear y to zero
        for (ii <- x.indices) {
          x(ii) = y(ii)
          y(ii) = 0.0
        }

        if (diff < epsilon ) {
          println("iterations completed:" + ctr)
          break
        }
      }
    }
    println("solution b = ")
    printVector(x)
    x
  }

  def prettyPrintAxB(a : Array[Array[Double]], b: Array[Double]) : Unit = {
    val z = new StringBuffer()
    for {
      j <- b.indices
    } {
      for {
        k <- a.indices
      } z.append(f" | ${a(j)(k)}%-15.5f" ) // x^${a.length - k}%2i + ")
      z.append(f" = ${b(j)}%8.4f  \n")
    }
    println(z)
  }

  // print out a 2-dimensional array with formatting
  def printArray[A](inArray :Array[Array[A]]): Unit = {
    val outBuffer = new StringBuffer()
    for(i <- inArray) yield {
      for(j <- i) yield {
        j match {
          case j:Integer => outBuffer.append(f"| ${j.toInt}%- 5d ")
          case j:Double => outBuffer.append(f"|  ${j.toDouble}%- 15.5f  ")
        }
      }
      outBuffer.append("|\n")
    }
    println(outBuffer)
  }


  // print out a 1-dimensional array with formatting
  def printVector[A](inArray :Array[A]): Unit = {
    val outBuffer = new StringBuffer()
      for(j <- inArray ) yield {
        j match {
          case j:Integer => outBuffer.append(f"|${j.toInt}%- 5d ")
          case j:Double => outBuffer.append(f"|${j.toDouble}%- 15.5f  ")
        }

    }
    outBuffer.append("|\n")
    println(outBuffer)
  }

  def main(args: Array[String]): Unit = {

    // find solution for system with 3 equations
    val a3 = Array.ofDim[Double](3, 3)
    val d3 = Array.ofDim[Double](3)
    a3(0)(0) = 4.0
    a3(0)(1) = -1.0
    a3(0)(2) = 0.0
    a3(1)(0) = -1.0
    a3(1)(1) = 4.0
    a3(1)(2) = -1.0
    a3(2)(0) = 0.0
    a3(2)(1) = -1.0
    a3(2)(2) = 4.0
    d3(0) = 2.0
    d3(1) = 6.0
    d3(2) = 2.0
    val aa3 = Array.ofDim[Double](3,3)
    for(i <- aa3.indices) aa3(i) = a3(i)
    val dd3 = Array.ofDim[Double](3)
    for(i <- dd3.indices) dd3(i) = d3(i)
    gaussSeidel(aa3,dd3,0.000001)
    gaussianElimination(a3,d3)
    prettyPrintAxB(a3,d3)


    val z2 = Array.ofDim[Double](3, 3)
    z2(0)(0) = 3.0
    z2(0)(1) = 4.0
    z2(0)(2) = 3.0
    z2(1)(0) = 2.0
    z2(1)(1) = -2.0
    z2(1)(2) = 4.0
    z2(2)(0) = -3.0
    z2(2)(1) = 1.0
    z2(2)(2) = 4.0

    // Use Conjugate Gradient Solver
    //conjugateGradientSolver(z2,d3,0.00001)

    val x2 = Array.ofDim[Double](2, 2)
    x2(0)(0) = 2.0
    x2(0)(1) = 1.0
    x2(1)(0) = 2.0
    x2(1)(1) = 1.0

    val d2 = Array.ofDim[Double](2)
    d2(0) = 2.0
    d2(1) = 4.0
    // use conjugate gradient solver
    //conjugateGradientSolver(x2,d2,0.000001)

    // gaussian elimination
    //gaussianElimination(x2,d2)
    // use gauss-seidel
   //gaussSeidel(x2,d2,0.00001)


  }
}
