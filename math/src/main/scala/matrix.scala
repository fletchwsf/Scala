/**
  * Created by wsf on 4/15/2017.
  */

import scala.collection.mutable.ArrayBuffer

import util.control.Breaks._

import math._

object  matrix {

  def addVectors(x: Array[Double], y : Array[Double]) : Array[Double] = {
    var z = Array.ofDim[Double](x.length)
    for {
      i <- x.indices
    } z(i) = x(i) + y(i)
    z
  }

  def subVectors(x: Array[Double], y : Array[Double]) : Array[Double] = {
    var z = Array.ofDim[Double](x.length)
    for {
      i <- x.indices
    } z(i) = x(i) - y(i)
    z
  }

  def add(x : Array[Array[Double]], y : Array[Array[Double]]): Array[Array[Double]] = {
    var z = Array.ofDim[Double](x.length, y.length)
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
    var z = Array.ofDim[Double](x.length, y.length)
    for {
      i <- x.indices
      j <- x.indices
    } z(i)(j) = x(i)(j) - y(i)(j)
    z
  }

  def multiply(x : Array[Array[Double]], y : Array[Array[Double]]): Array[Array[Double]] = {
    var z = Array.ofDim[Double](x.length, y.length)
    for {
      i <- x.indices
      j <- x.indices
      k <- y.indices
    } z(i)(j) += x(i)(j) * y(k)(j)
    z
  }

  def gaussianElimination(a : Array[Array[Double]], d : Array[Double]): Array[Double] = {
    println("----------------------------------------")
    println("Gassuian Elimination z,d")
    println("----------------------------------------")
    prettyPrintAxB(a,d)
    var x = Array.ofDim[Double](a.length)
    val n = a.length -1
    for ( i <- 0 to n-1 ){
      for (k <- i + 1 to n){
        var m = a(k)(i) / a(i)(i)
        a(k)(i) = 0
        for (j <- i + 1 to n) {
          a(k)(j) -= m * a(i)(j)
        }
        d(k) -= m * d(i)
      }
    }
    // back substitution
    for (ii <- a.indices ) {
      var i = n - ii
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
    prettyPrintXY(a,b)

    var convergence = false
    // temporary values
    var gg1 : Double = 0.0
    // define arrays
    var ad = Array.ofDim[Double](b.length)
    var x = Array.ofDim[Double](b.length)
    var g = Array.ofDim[Double](b.length)
    var d = Array.ofDim[Double](b.length)
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
    var x = Array.ofDim[Double](d.length)
    for (j <- x.indices) x(j) = 0.0
    var y = Array.ofDim[Double](a.length)
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

  def printVector(a : Array[Double]): Unit = {
    var s = new StringBuffer()
    for {
         i <- a.indices                                   // return the start and end indices of the vector
       } s.append(f" | ${a(i)}%-15.5f")
      //println(s"($i) = ${a(i)}")
    s.append("\n")
    println(s)
  }

  def prettyPrintDim2(a: Array[Array[Double]]) : Unit = {
    var s = new StringBuffer()
    for ( i <-  a.indices) {
      for ( j <- a.indices) s.append(f" | ${a(i)(j)}%-15.5f" )
    s.append("\n")
    }
    println(s)
  }

  def prettyPrintXY(x : Array[Array[Double]], y : Array[Double]) : Unit = {
    var z = new StringBuffer()
    for {
      j <- y.indices
    } {
      for {
        k <- x.indices
      } z.append(s" x($j)($k) ${x(j)(k)}")
      z.append(s" | y($j) ${y(j)} | \n")
    }
    println(z)
  }

  def prettyPrintAxB(a : Array[Array[Double]], b: Array[Double]) : Unit = {
    var z = new StringBuffer()
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


    var z2 = Array.ofDim[Double](3, 3)
    z2(0)(0) = 3.0
    z2(0)(1) = 4.0
    z2(0)(2) = 3.0
    z2(1)(0) = 2.0
    z2(1)(1) = -2.0
    z2(1)(2) = 4.0
    z2(2)(0) = -3.0
    z2(2)(1) = 1.0
    z2(2)(2) = 4.0

//    val d3 = Array.ofDim[Double](3)
//    d3(0) = 3.0
//    d3(1) = 12.0
//    d3(2) = 16.0
//    gaussSeidel(z2,d3,0.000001)

    // Use Conjugate Gradient Solver
    //conjugateGradientSolver(z2,d3,0.00001)

    var x2 = Array.ofDim[Double](2,2)
    x2(0)(0) = 2.0
    x2(0)(1) = 1.0
    x2(1)(0) = 2.0
    x2(1)(1) = 1.0

    var d2 = Array.ofDim[Double](2)
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
