package sql2json
package testing
package laws 

import cat.Cofunctor
import cat.Cofunctor.given
import cat.Eq
import cat.Show
import cat.Applicative.~
import testing.Arbitrary
import testing.Arbitrary.forAll
import testing.Result.given
import org.junit.Test

abstract class CofunctorLaws[F[_], A, B, C](given FG: CofunctorLaws.Givens[F, A, B, C])

  @Test def identityLaw(): Unit = FG.run {
      forAll[F[A]]("identity over comap") { fa => 
        fa <-> fa.comap(identity(_: A))
      }
    }

  @Test def compositionLaw(): Unit = FG.run {
    forAll[F[A] ~ (B => A) ~ (C => B)]("cmposition over comap") {
      case fa ~ b2a ~ c2b => fa.comap(b2a).comap(c2b) <-> fa.comap(c2b andThen b2a)
    }
  }

object CofunctorLaws
  class Givens[F[_], A, B, C](given
    Cofunctor[F],
    Show[F[A]],
    Show[F[C]],
    Eq[F[A]],
    Eq[F[C]],
    Eq[C],
    Arbitrary[F[A]],
    Arbitrary[B => A],
    Arbitrary[C => B]
  ) with
    def run(body: (given Cofunctor[F], Show[F[A]], Show[F[C]], Eq[F[A]], Eq[F[C]], Eq[C], Arbitrary[F[A]], Arbitrary[B => A], Arbitrary[C => B]) => Unit): Unit = 
      body.apply