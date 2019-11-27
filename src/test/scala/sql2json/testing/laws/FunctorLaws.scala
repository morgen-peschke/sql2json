package sql2json
package testing
package laws 

import cat.Functor
import cat.Functor.given
import cat.Eq
import cat.Show
import cat.Applicative.~
import testing.Arbitrary
import testing.Arbitrary.forAll
import testing.Result.given
import org.junit.Test

abstract class FunctorLaws[F[_], A, B, C](given FG: FunctorLaws.Givens[F, A, B, C])

  @Test def identityLaw(): Unit = FG.run {
    forAll[F[A]]("identity over map") { fa => 
      fa <-> fa.map(identity)
    }
  }

  @Test def compositionLaw(): Unit = FG.run {
    forAll[F[A] ~ (A => B) ~ (B => C)]("cmposition over map") {
      case fa ~ a2b ~ b2c => fa.map(a2b).map(b2c) <-> fa.map(a2b andThen b2c)
    }
  }

object FunctorLaws
  class Givens[F[_], A, B, C](given
    Functor[F],
    Show[F[A]],
    Show[F[C]],
    Eq[F[A]],
    Eq[F[C]],
    Arbitrary[F[A]],
    Arbitrary[A => B],
    Arbitrary[B => C]
  ) with
    def run(body: (given Functor[F], Show[F[A]], Show[F[C]], Eq[F[A]], Eq[F[C]], Arbitrary[F[A]], Arbitrary[A => B], Arbitrary[B => C]) => Unit): Unit = 
      body.apply