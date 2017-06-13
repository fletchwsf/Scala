package FiniteElementAnalysis

/**
  * Created by wsf on 6/11/2017.
  */
object calculateStress {

  def stress(connectionTable : Array[Array[Integer]], eDisplacement : Array[Double], eLength: Array[Double], eModulus : Array[Double]) : Array[Double] = {

    val sigma = Array.ofDim[Double](eLength.length)

      for(i <- sigma.indices)
        // todo Replace hardcoded 1,0 with the correct generalized indices value
        sigma(i) = eModulus(i) / eLength(i) * (eDisplacement(connectionTable(i)(1)) - eDisplacement(connectionTable(i)(0)) )

    sigma
  }

}
