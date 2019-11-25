package sql2json
package testing
package laws 

import cat.Functor
import cat.Functor.given
import cat.Eq
import cat.Eq.given
import cat.Show
import cat.Applicative.~
import testing.Arbitrary
import testing.Arbitrary.forAll
import testing.Result.given
import org.junit.Test

abstract class FunctorLaws[F[_], A, B, C](given FG: FunctorLaws.Givens[F, A, B, C])

  @Test def functorIdentity(): Unit = FG.run {
      forAll[F[A]]("identity") { fa => 
        fa <-> fa.map(identity)
      }
    }

  @Test def functorComposition(): Unit = FG.run {
    forAll[F[A] ~ (A => B) ~ (B => C)]("cmposition") {
      case fa ~ a2b ~ b2c => fa.map(a2b).map(b2c) <-> fa.map(a2b andThen b2c)
    }
  }

object FunctorLaws
  class Givens[F[_], A, B, C]( 
    ff: Functor[F],
    sfa: Show[F[A]],
    sfc: Show[F[C]],
    efa: Eq[F[A]],
    efc: Eq[F[C]],
    ec: Eq[C],
    afa: Arbitrary[F[A]],
    aa2b: Arbitrary[A => B],
    ab2c: Arbitrary[B => C]
  ) with
    def run(body: (given Functor[F], Show[F[A]], Show[F[C]], Eq[F[A]], Eq[F[C]], Eq[C], Arbitrary[F[A]], Arbitrary[A => B], Arbitrary[B => C]) => Unit): Unit = 
      body(given ff, sfa, sfc, efa, efc, ec, afa, aa2b, ab2c)

  object Givens
      def derive[F[_], A, B, C](given
        ff: Functor[F],
        sfa: Show[F[A]],
        sfc: Show[F[C]],
        efa: Eq[F[A]],
        efc: Eq[F[C]],
        ec: Eq[C],
        afa: Arbitrary[F[A]],
        aa2b: Arbitrary[A => B],
        ab2c: Arbitrary[B => C]
      ): Givens[F,A,B,C] = 
        Givens(ff, sfa, sfc, efa, efc, ec, afa, aa2b, ab2c)