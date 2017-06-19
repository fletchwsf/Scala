package FiniteElementAnalysis

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Created by wsf on 6/11/2017.
  */
object runnerSingleDOF {


  def main(args: Array[String]): Unit = {

    // file name from command line
    // todo insert the file name from the command line argument
    val inputFileName = "D:\\Scala\\math\\src\\test\\scala\\test_FiniteElementAnalysis\\FExample_2.txt"

    // read the input file into memory
    val inputFile = new ListBuffer[String]
    val bufferedSource = Source.fromFile(inputFileName)
    for (line <- bufferedSource.getLines)
      inputFile += line
    bufferedSource.close

    // read in the nInput list that defines the general values for the model
    val nInput = IO.nInputValues(inputFile)

    // read the degree-of-freedom value for the model

    // read the number of elements in the model

    // read the list of nodes with single point constraints

    // read the element connection table matrix

    // read the element cross-sectional area table

    // read the element length table

    // read the element modulus of elasticity table

    // read the element body force table

    // read the multi-point constraint array matrix

    // build the initial stiffness matrix array

    // establish the penalty stiffness value

    // add the single-point constraint values to the stiffness matrix array

    // add the multi-point constraint values to the stiffness matrix array

    // solve for the element displacement vector

    // solve for the element stress vector

  }
}
