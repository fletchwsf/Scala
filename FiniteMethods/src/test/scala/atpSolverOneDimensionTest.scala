import org.scalatest.FunSuite
import org.scalatest.Matchers._

/**
  * Created by wsf on 6/24/2017.
  */
class atpSolverOneDimensionTest extends FunSuite {

  test("test Example 3-3") {
    val testFile = "D:\\Scala\\FiniteMethods\\src\\test\\scala\\support\\FExample_3-3.txt"
    println("starting test")
    println(s"Test File $testFile")
    val result = solverOneDimension.solverOneDOF(testFile)
    var answer = Array.ofDim[Double](3)
    answer(0) = 0.00000000056
    answer(1) = 0.000009272
    answer(2) = 0.000009953

    assert(answer.length === result.length, "the result array length of $result.length, was unexpected")
    val tolerance = 0.1
    for ( i <- result.indices)
      assert(result(i) === answer(i) +- answer(i)*tolerance , "example 3.3 displacement calculation is wrong")

  }


  test("test Example 3-5-1") {
    // first attempt with end not constrained
    val testFile = "D:\\Scala\\FiniteMethods\\src\\test\\scala\\support\\FExample_3-5-1.txt"
    println("starting test")
    println(s"Test File $testFile")
    val result_1 = solverOneDimension.solverOneDOF(testFile)
    // the initial displacement of element 2 is 1.8 mm
    assert(result_1(1) === 1.8 +- 0.0018, "example 3-5 step 1 result should be 1.8")
  }

  test("test Example 3-5-2") {
    // Second attempt with location of element 3 constrained
    val testFile = "D:\\Scala\\FiniteMethods\\src\\test\\scala\\support\\FExample_3-5-2.txt"
    println("starting test")
    println(s"Test File $testFile")
    val result_2 = solverOneDimension.solverOneDOF(testFile)
    var answer = Array.ofDim[Double](3)
    answer(0) = 7.49985 * 0.00001
    answer(1) = 1.500045
    answer(2) = 1.2

    val tolerance = 0.1
    for ( i <- result_2.indices)
      assert(result_2(i) === answer(i) +- answer(i)*tolerance , "example 3.6 displacement calculation is wrong")

  }


  test("test Example 3-6") {
    val testFile = "D:\\Scala\\FiniteMethods\\src\\test\\scala\\support\\FExample_3-6.txt"
    println("starting test")
    println(s"Test File $testFile")
    val result = solverOneDimension.solverOneDOF(testFile)
    var answer = Array.ofDim[Double](5)
    answer(0) = 0.48354
    answer(1) = 1.20964
    answer(2) = 0.00005
    answer(3) = 0.00005
    answer(4) = 1.45221

    assert(answer.length === result.length, "the result array length of $result.length, was unexpected")
    val tolerance = 0.1
    for ( i <- result.indices)
      assert(result(i) === answer(i) +- answer(i)*tolerance , "example 3.6 displacement calculation is wrong")

  }


  test("test Example 3-7") {
    val testFile = "D:\\Scala\\FiniteMethods\\src\\test\\scala\\support\\FExample_3-7.txt"
    println("starting test")
    println(s"Test File $testFile")
    val result = solverOneDimension.solverOneDOF(testFile)
    var answer = Array.ofDim[Double](5)
    answer(0) = 0.000001
    answer(1) = 0.5735  / 10000.0
    answer(2) = 0.10706 / 1000.0
    answer(3) = 1.41147 / 10000.0
    answer(4) = 1.5294  / 10000.0

    assert(answer.length === result.length, "the result array length of $result.length, was unexpected")
    val tolerance = 0.001
    for ( i <- result.indices)
      assert(result(i) === answer(i) +- tolerance , "example 3.7 displacement calculation is wrong")

  }



}
