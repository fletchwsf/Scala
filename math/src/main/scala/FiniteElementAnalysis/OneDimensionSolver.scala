package FiniteElementAnalysis

/**
  * Created by wsf on 5/28/2017.
  */

import scala.collection.mutable._
import scala.io.Source

import FiniteElementAnalysis.IO._
import matrix._

object OneDimensionSolver {




}


object Main extends App {

  var inputFileName = "D:\\Scala\\math\\src\\test\\scala\\test_FiniteElementAnalysis\\FExample_2.txt"

  // read the input file into memory
  val inputFile = new ListBuffer[String]
  val bufferedSource = Source.fromFile(inputFileName)
  for (line <- bufferedSource.getLines)
    inputFile += line
  bufferedSource.close


  // nInput - The number parameters that define attributes of the problem and finite element arrays
  var nInput = scala.collection.mutable.Map[String,Integer]()
  nInput += (("NN",-1), ("NE",-1), ("NM",-1), ("NDIM",-1),("NEN",-1), ("NDM",-1), ("ND",-1), ("NL",-1), ("NMPC",-1))
  nInput += (("NQ", nInput("NN") * nInput("NDM")  ))

  var nInputArray = FiniteElementAnalysis.IO.readInts("nInput", inputFile)
  nInput.update("NN",nInputArray(0))
  nInput.update("NE",nInputArray(1))
  nInput.update("NM",nInputArray(2))
  nInput.update("NDIM",nInputArray(3))
  nInput.update("NDM",nInputArray(4))
  nInput.update("NEN",nInputArray(5))
  nInput.update("ND",nInputArray(6))
  nInput.update("NL",nInputArray(7))
  nInput.update("NMPC",nInputArray(8))
  nInput += (("NQ", nInput("NN") * nInput("NDM")  ))
  for(e <- nInput) println(e.toString())

  //connectionTable = loadArray("connection", elementCount, inputFile)
  //#ConnectionArray(NM,NDIM+1)
  var connectionArray = Array.ofDim[Int](nInput("NM"), nInput("NDM")+1)

}

