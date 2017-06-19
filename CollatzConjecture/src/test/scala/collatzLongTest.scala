/**
  * Created by wsf on 6/19/2017.
  */

// A example of a property based test using ScalaCheck


import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object collatzLongTest extends Properties("collatz") {

  val bigInt  = Gen.choose(1,2147483647)

  val p1 =  forAll(bigInt) { n =>
    collatz.conjecture(n).nonEmpty  }
  p1.check

  val p2 =  forAll(bigInt) { n =>
    collatz.conjecture(n).last == 1  }
  p2.check

}


