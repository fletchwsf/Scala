package FiniteElementAnalysis

/**
  * Created by wsf on 5/31/2017.
  */

import scala.collection.mutable.ListBuffer
import scala.io.Source
import matrix._

object stiffnessMatrix {

  // Build a stiffness matrix with no constraints
  def kBuild( con : Array[Array[Int]],
              DOF : Int,
              localDOF : Int,
              eArea : Array[Double],
              eLength : Array[Double],
              eModulus : Array[Double]
            ) : Array[Array[Double]] = {

    val K = Array.ofDim[Double](DOF, DOF)

    // define the element strain displacement matrix - eSDM
    val eSDM = Array.ofDim[Int](localDOF, localDOF)
    localDOF match {
      case 2 =>              // eSDM for an element with linear shape function
        eSDM (0) (0) = 1
        eSDM (0) (1) = - 1
        eSDM (1) (0) = - 1
        eSDM (1) (1) = 1
      case 3 =>             // eSDM for an element with a quadratic shape function
        eSDM (0) (0) = 7
        eSDM (0) (1) = 1
        eSDM (0) (2) = - 8
        eSDM (1) (0) = 1
        eSDM (1) (1) = 7
        eSDM (1) (2) = - 8
        eSDM (2) (0) = - 8
        eSDM (2) (1) = - 8
        eSDM (2) (2) = 16
    }

    // build stiffness matrix with no constraints
    for (i <- con.indices) {
        for (x <- eSDM.indices) {
          for (y <- eSDM.indices) {
            K(con(i)(x))(con(i)(y)) +=  eSDM(x)(y) * 1.0 * eModulus(i) * eArea(i) / eLength(i)
          }
        }
    }
    K
  }

}
