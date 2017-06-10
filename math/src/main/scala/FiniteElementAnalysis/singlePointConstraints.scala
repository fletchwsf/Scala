package FiniteElementAnalysis

/**
  * Created by wsffl on 6/10/2017.
  */
object singlePointConstraints {


  def addSinglePointConstraints(
                                 kMatrix : Array[Array[Double]],
                                 constraints : Array[Int]
                                 ): Array[Array[Double]] = {

    val C : Double  = matrix.max(kMatrix) * 10000.0

    for(i <- constraints.indices)
      kMatrix(constraints(i))(constraints(i)) += C
    kMatrix
  }
}
