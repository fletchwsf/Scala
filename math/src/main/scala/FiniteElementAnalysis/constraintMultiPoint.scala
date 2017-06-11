package FiniteElementAnalysis

/**
  * Created by wsf on 6/10/2017.
  */
object constraintMultiPoint {

  def add(  K2 : Array[Array[Double]],
            C: Double,
            nodesArray: Array[Array[Integer]],
            qRatios: Array[Double]
         ): Array[Array[Double]] = {

    val B = Array.ofDim[Double](3)
    B(0) = 0.0

    var kmOne = Array.ofDim[Double](2,2)

    for (i <- nodesArray.indices) {
      B(1) = 1.0
      B(2) = qRatios(i)
      kmOne(0)(0) = C*B(1)*B(1)
      kmOne(0)(1) = C*B(1)*B(2)
      kmOne(1)(0) = C*B(2)*B(1)
      kmOne(1)(1) = C*B(2)*B(2)

      for (j <- nodesArray.indices)
        for (k <- nodesArray.indices) {
          //K2(nodesArray(i)(j))(nodesArray(i)(k)) += kmOne(i)(j)
         // println(s"K2 numbering: ${nodesArray(i)(j)}${nodesArray(i)(k)} += ${kmOne(j)(k)}")
          K2(nodesArray(i)(j))(nodesArray(i)(k)) += kmOne(j)(k)
        }
    }
    K2
  }



}
