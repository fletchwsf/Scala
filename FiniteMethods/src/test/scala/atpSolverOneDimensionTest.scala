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
    answer(0) = 0.0
    answer(1) = 0.5735
    answer(2) = 0.10706
    answer(3) = 1.41147
    answer(4) = 1.5294

    assert(answer.length === result.length, "the result array length of $result.length, was unexpected")
    val tolerance = 0.1
    for ( i <- result.indices)
      assert(result(i) === answer(i) +- answer(i)*tolerance , "example 3.7 displacement calculation is wrong")

  }



}
