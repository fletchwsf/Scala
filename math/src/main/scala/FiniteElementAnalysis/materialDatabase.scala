package FiniteElementAnalysis

/**
  * Created by wsf on 5/28/2017.
  */


// modulus("steel-Eng") = 1E106 psi
// density("steel-SI") = 0.5 kg/m3

//  name,   units,  modulus,  density
//  steel,  Eng,    1E106,    30.0
//  steel,  SI,     2.4E106,  15.0

class MaterialDatabase {

  class properties {
    protected var name: String = ""
    protected var modulus: Double = 0
    protected var density: Double = 0
  }

  //def data(entries: Array[properties]):

}



//  def setNN(numberOfNodes : Int): Int = {
//    kNN = numberOfNodes
//    kNN
//  }
//
//  def getNN : Int = kNN

