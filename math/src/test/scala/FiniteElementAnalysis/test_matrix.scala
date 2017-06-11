package FiniteElementAnalysis

/**
  * Created by wsf on 4/15/2017.
  */

import org.scalatest._

class test_matrix extends FunSuite {

  val a1 : Array[Array[Double]] = Array.ofDim[Double](1,1)
  a1(0)(0) = 1.0

  val b1 : Array[Array[Double]] = Array.ofDim[Double](1,1)
  b1(0)(0) = 2.0

 test("Add  1x1") {
   val z1 = matrix.add(a1,b1)
   val result  = Array.ofDim[Double](1,1)
   result(0)(0) = 3.0
   assert(z1 === result, "fail")

 }

  test("Add vector - 01") {
    val v1 : Array[Double] = Array.ofDim[Double](1)
    v1(0) = 1.0
    val v2 : Array[Double] = Array.ofDim[Double](1)
    v2(0) = 2.0
    val result : Array[Double] = Array.ofDim[Double](1)
    result(0) = 3.0
    assert(result === matrix.addVectors(v1,v2), "fail" )
  }

  test("Add vector - 02") {
    val v1 : Array[Double] = Array.ofDim[Double](2)
    v1(0) = 1.0
    v1(1) = 2.0
    val v2 : Array[Double] = Array.ofDim[Double](2)
    v2(0) = 2.0
    v2(1) = 3.0
    val result : Array[Double] = Array.ofDim[Double](2)
    result(0) = 3.0
    result(1) = 5.0
    assert(result === matrix.addVectors(v1,v2), "fail" )
    assert(3.0 === matrix.sumOf(v1), "fail" )
    assert(0.0 === matrix.sumOf(matrix.subVectors(v1,v1)), "fail")
  }

  test("mult 1x1"){
    val z1 = matrix.add(a1,b1)
    val result  = Array.ofDim[Double](1,1)
    result(0)(0) = 3.0
    assert(z1 === result, "fail")
  }

  val x2 : Array[Array[Double]] = Array.ofDim[Double](2,2)
  x2(0)(0) = 1.0
  x2(0)(1) = 2.0
  x2(1)(0) = 3.0
  x2(1)(1) = 4.0

  val y2 : Array[Array[Double]] = Array.ofDim[Double](2,2)
  y2(0)(0) = 5.0
  y2(0)(1) = 6.0
  y2(1)(0) = 7.0
  y2(1)(1) = 8.0

  test("Add 2x2") {
    val z2 = matrix.add(x2,y2)
    val result  = Array.ofDim[Double](2,2)
    result(0)(0) = 6.0
    result(0)(1) = 8.0
    result(1)(0) = 10.0
    result(1)(1) = 12.0
    assert(z2 === result, "fail")
  }

  test("Sub 2x2") {
    val z2 = matrix.subtract(x2,y2)
    val result  = Array.ofDim[Double](2,2)
    result(0)(0) = -4.0
    result(0)(1) = -4.0
    result(1)(0) = -4.0
    result(1)(1) = -4.0
    assert(z2 === result, "fail")
  }

  test("multi 2x2") {
    val z2 = matrix.multiply(x2,y2)
    val result  = Array.ofDim[Double](2,2)
    result(0)(0) = 12.0
    result(0)(1) = 28.0
    result(1)(0) = 36.0
    result(1)(1) = 56.0
    assert(z2 === result, "fail")

  }

  test("gaussian elimination - 1") {
    val y : Array[Array[Double]] = Array.ofDim[Double](3, 3)
    y(0)(0) = 1.0
    y(0)(1) = -2.0
    y(0)(2) = 6.0
    y(1)(0) = 2.0
    y(1)(1) = 2.0
    y(1)(2) = 3.0
    y(2)(0) = -1.0
    y(2)(1) = 3.0
    y(2)(2) = 0.0

    val d : Array[Double] = Array.ofDim[Double](3)
    d(0) = 0.0
    d(1) = 3.0
    d(2) = 2.0

    val z2 = matrix.gaussianElimination(y, d)
    val result = Array.ofDim[Double](3)
    result(0) = 2.0 / 5.0
    result(1) = 4.0 / 5.0
    result(2) = 1.0 / 5.0

    assert(matrix.sumOf(matrix.subVectors(z2, result)) <= 0.3, "failed check with tolerance")
  }

    test("gaussian elimination - 2") {
      var y = Array.ofDim[Double](3, 3)
      y(0)(0) = 2.0
      y(0)(1) = 1.0
      y(0)(2) = -1.0
      y(1)(0) = -3.0
      y(1)(1) = -1.0
      y(1)(2) = 2.0
      y(2)(0) = -2.0
      y(2)(1) = 1.0
      y(2)(2) = 2.0

      val d = Array.ofDim[Double](3)
      d(0) = 8.0
      d(1) = -11.0
      d(2) = -3.0

      val z2 = matrix.gaussianElimination(y, d)
      val result = Array.ofDim[Double](3)
      result(0) = 2.0
      result(1) = 3.0
      result(2) = -1.0

      assert(matrix.sumOf(matrix.subVectors(z2, result)) <= 0.3, "failed check with tolerance")
    }

    test("Gauss-Seidel-1 ") {
      val a3 : Array[Array[Double]] = Array.ofDim[Double](8,8)
      a3(0)(0) = 6.0
      a3(0)(1) = 0.0
      a3(0)(2) = 1.0
      a3(0)(3) = 2.0
      a3(0)(4) = 0.0
      a3(0)(5) = 0.0
      a3(0)(6) = 2.0
      a3(0)(7) = 1.0

      a3(1)(0) = 0.0
      a3(1)(1) = 5.0
      a3(1)(2) = 1.0
      a3(1)(3) = 1.0
      a3(1)(4) = 0.0
      a3(1)(5) = 0.0
      a3(1)(6) = 3.0
      a3(1)(7) = 0.0

      a3(2)(0) = 1.0
      a3(2)(1) = 1.0
      a3(2)(2) = 6.0
      a3(2)(3) = 1.0
      a3(2)(4) = 2.0
      a3(2)(5) = 0.0
      a3(2)(6) = 1.0
      a3(2)(7) = 2.0

      a3(3)(0) = 2.0
      a3(3)(1) = 1.0
      a3(3)(2) = 1.0
      a3(3)(3) = 7.0
      a3(3)(4) = 1.0
      a3(3)(5) = 2.0
      a3(3)(6) = 1.0
      a3(3)(7) = 1.0

      a3(4)(0) = 0.0
      a3(4)(1) = 0.0
      a3(4)(2) = 2.0
      a3(4)(3) = 1.0
      a3(4)(4) = 6.0
      a3(4)(5) = 0.0
      a3(4)(6) = 2.0
      a3(4)(7) = 1.0

      a3(5)(0) = 0.0
      a3(5)(1) = 0.0
      a3(5)(2) = 0.0
      a3(5)(3) = 2.0
      a3(5)(4) = 0.0
      a3(5)(5) = 4.0
      a3(5)(6) = 1.0
      a3(5)(7) = 0.0

      a3(6)(0) = 2.0
      a3(6)(1) = 3.0
      a3(6)(2) = 1.0
      a3(6)(3) = 1.0
      a3(6)(4) = 2.0
      a3(6)(5) = 1.0
      a3(6)(6) = 5.0
      a3(6)(7) = 1.0

      a3(7)(0) = 1.0
      a3(7)(1) = 0.0
      a3(7)(2) = 2.0
      a3(7)(3) = 1.0
      a3(7)(4) = 1.0
      a3(7)(5) = 0.0
      a3(7)(6) = 1.0
      a3(7)(7) = 3.0

      val d3 : Array[Double] = Array.ofDim[Double](8)
      d3(0) = 1.0
      d3(1) = 1.0
      d3(2) = 1.0
      d3(3) = 1.0
      d3(4) = 1.0
      d3(5) = 1.0
      d3(6) = 1.0
      d3(7) = 1.0

      val r3 : Array[Double] = Array.ofDim[Double](8)
      r3(0) = 0.39255
      r3(1) = 0.63974
      r3(2) = -0.14303
      r3(3) = -0.21723
      r3(4) = 0.38019
      r3(5) = 0.51182
      r3(6) = -0.61281
      r3(7) = 0.44779

      val z3 = matrix.gaussSeidel(a3,d3,0.0001)
      assert(matrix.sumOf(matrix.subVectors(z3, r3)) <= 0.001, "failed check with tolerance")

    }


}
