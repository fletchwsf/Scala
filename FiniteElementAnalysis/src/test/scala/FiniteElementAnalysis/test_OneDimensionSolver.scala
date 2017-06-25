package FiniteElementAnalysis

import org.scalatest.FunSuite

/**
  * Created by wsf on 5/28/2017.
  */

class test_OneDimensionSolver extends  FunSuite{


//  #NN     NE      NM      NDIM    NEN     NDN     ND     NL       NMPC    --- 1 line of data nine entries
//    5,      2,      2,      2,      2,      1,      2,     1,       2

  var nInput = scala.collection.mutable.Map[String,Int]()
  nInput += (("NN",5), ("NE",2), ("NM",2), ("NDIM",2),("NEN",2), ("NDM",1), ("ND",1), ("NL",1), ("NMPC",2))
  nInput += (("NQ", nInput("NN") * nInput("NDM")  ))

  test("reading input definitions") {
    var newInput = scala.collection.mutable.Map[String,Int]()
    assert(nInput === nInput, "")
  }



}
