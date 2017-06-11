package FiniteElementAnalysis

/**
  * Created by wsf on 6/11/2017.
  */
object calculateStress {

  def stress(displacement : Array[Double], modulus : Array[Double]) : Array[Double] = {

    println("DUMMY FUNCTION IN FILE: calculateStress NEEDS TO BE FIXED HERE ")
    val sigma = Array.ofDim[Double](displacement.length)

      for(i <- sigma.indices)
        sigma(i) = displacement(i)

    sigma
  }

}
