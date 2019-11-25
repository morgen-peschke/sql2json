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

  @Test def eqReflexitivityCheck(): Unit = givensForEqLaws.run {
    forAll[A]("reflexivity")(x => x <-> x)
  }

  @Test def eqSymmetryCheck(): Unit = givensForEqLaws.run {
    forAll[A ~ A]("symmetry") {
      case x ~ y => (x === y) <-> (y === x)
    }
  }

  @Test def eqTransitivityCheck(): Unit = givensForEqLaws.run {
    forAll[A ~ A ~ A]("transitivity"){ 
      case x ~ y ~ z => ((x =!= y || y =!= z) || ((x === y) && (y === z) && (x === z))) <-> true
    }
  }

object EqLaws
  class Givens[A](eqForA: Eq[A], showForA: Show[A], arbitraryA: Arbitrary[A])
    def run(body: (given Eq[A], Show[A], Arbitrary[A]) => Unit): Unit = 
        body(given eqForA, showForA, arbitraryA)

  object Givens
    def apply[A](given E: Eq[A], S: Show[A], A: Arbitrary[A]): Givens[A] = Givens(E, S, A)
