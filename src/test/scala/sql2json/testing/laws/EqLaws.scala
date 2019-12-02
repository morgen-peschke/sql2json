package sql2json
package testing
package laws

import cat.Eq
import cat.Eq.given
import cat.Show
import cat.Applicative.~
import testing.Arbitrary
import testing.Arbitrary.{forAll, given}
import testing.Result.given
import org.junit.Test

abstract class EqLaws[A](given givensForEqLaws: EqLaws.Givens[A])

  @Test def reflexitivityLaw(): Unit = givensForEqLaws.run {
    forAll[A]("reflexive equality")(x => x <-> x)
  }

  @Test def symmetryLaw(): Unit = givensForEqLaws.run {
    forAll[A ~ A]("symmetric equality") {
      case x ~ y => (x === y) <-> (y === x)
    }
  }

  @Test def transitivityLaw(): Unit = givensForEqLaws.run {
    forAll[A ~ A ~ A]("transitive equality"){ 
      case x ~ y ~ z => ((x =!= y || y =!= z) || ((x === y) && (y === z) && (x === z))) <-> true
    }
  }

object EqLaws
  class Givens[A](given Eq[A], Show[A], Arbitrary[A])
    def run(body: (given Eq[A], Show[A], Arbitrary[A]) => Unit): Unit = body.apply

  given [A: Eq: Show: Arbitrary]: Givens[A]