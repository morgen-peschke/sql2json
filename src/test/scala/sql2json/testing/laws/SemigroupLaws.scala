package sql2json
package testing
package laws

import cat.{Show, Eq, Semigroup}
import cat.Applicative.{~, given}
import cat.Semigroup.given
import testing.Arbitrary
import testing.Arbitrary.forAll
import testing.Result.given
import org.junit.Test

abstract class SemigroupLaws[A](given SG: SemigroupLaws.Givens[A])
  @Test def associativeLaw(): Unit = SG.run {
    forAll[A ~ A ~ A]("combine is associative") {
      case a ~ b ~ c =>
        ((a combine b) combine c) <-> (a combine (b combine c))
    }
  }

object SemigroupLaws
  class Givens[A](given
    Semigroup[A],
    Arbitrary[A],
    Eq[A],
    Show[A]
  ) with
    def run(body: (given 
      Semigroup[A],
      Arbitrary[A],
      Eq[A],
      Show[A]
      ) => Unit): Unit = body.apply

  given[A](given
    Semigroup[A],
    Arbitrary[A],
    Eq[A],
    Show[A]
  ): Givens[A]