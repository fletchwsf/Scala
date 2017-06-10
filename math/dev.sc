import FiniteElementAnalysis.matrix._
import FiniteElementAnalysis.stiffnessMatrix


  val nElements = 2
  val eArea = Array(1.0, 1.0)
  val eLength =  Array (1.0, 1.0)
  val eModulus  = Array(1.0, 1.0)

  val nNodes = nElements + 1
  val nDOFperNode = 1
  val totalDOF = nNodes * nDOFperNode
  val localDOF = 3

  var con = Array.ofDim[Int](2,2)
  con(0)(0) = 0
  con(0)(1) = 1
  con(1)(0) = 1
  con(1)(1) = 2

  val example = stiffnessMatrix.kBuild(con, 4, localDOF, eArea, eLength, eModulus)
  var result = Array.ofDim[Double](4,4)
  result(0)(0) = 7.0
  result(0)(1) = 1.0
  result(0)(2) = -8.0
  result(0)(3) = 0.0
  result(1)(0) = 1.0
  result(1)(1) = 14.0
  result(1)(2) = -7.0
  result(1)(3) = -8.0
  result(2)(0) = -8.0
  result(2)(1) = -7.0
  result(2)(2) = 23.0
  result(2)(3) = -8.0
  result(3)(0) = 0.0
  result(3)(1) = -8.0
  result(3)(2) = -8.0
  result(3)(3) = 16.0