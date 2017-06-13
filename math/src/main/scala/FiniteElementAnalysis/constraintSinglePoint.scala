package FiniteElementAnalysis

/**
  * Created by wsf on 6/10/2017.
  */
object constraintSinglePoint {


  def add(
                                 kMatrix : Array[Array[Double]],
                                 constraints : Array[Integer]
                                 ): Array[Array[Double]] = {

    val C : Double  = matrix.max(kMatrix) * 10000.0

    for(i <- constraints.indices)
      kMatrix(constraints(i))(constraints(i)) += C
    kMatrix
  }
}
