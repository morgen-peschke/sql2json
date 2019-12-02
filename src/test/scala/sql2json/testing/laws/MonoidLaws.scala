package sql2json
package testing
package laws

import cat.{Show, Eq, Monoid}
import cat.Applicative.{~, given}
import cat.Semigroup.given
import cat.Monoid.empty
import testing.Arbitrary
import testing.Arbitrary.forAll
import testing.Result.given
import org.junit.Test

abstract class MonoidLaws[A](given MG: MonoidLaws.Givens[A])
  @Test def leftIdentity(): Unit = MG.run {
    forAll[A]("combine left identity check") { a => 
        (empty[A] combine a) <-> a
    }
  }

  @Test def rightIdentity(): Unit = MG.run {
    forAll[A]("combine right identity check") { a => 
        (a combine empty[A]) <-> a
    }
  }

object MonoidLaws
  class Givens[A](given
    Monoid[A],
    Arbitrary[A],
    Eq[A],
    Show[A]
  ) with
    def run(body: (given 
      Monoid[A],
      Arbitrary[A],
      Eq[A],
      Show[A]
      ) => Unit): Unit = body.apply

  given[A](
    given
      Monoid[A],
      Arbitrary[A],
      Eq[A],
      Show[A]
    ): Givens[A]