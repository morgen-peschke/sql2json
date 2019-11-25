package sql2json
package testing.laws

import cat.{Applicative, Functor, Show, Eq}
import cat.Applicative.{~, given}
import cat.Functor.given
import testing.Arbitrary
import testing.Arbitrary.forAll
import testing.Result.given
import org.junit.Test

abstract class ApplicativeLaws[F[_], A, B, C](given AG: ApplicativeLaws.Givens[F, A, B, C])

  @Test def applicativeIdentityLaw(): Unit = AG.run {
    forAll[F[A]]("applicative identity") { fa =>
      (identity[A] _).pure[F] <*> fa <-> fa
    }
  }

  @Test def applicativeHomomorphismLaw(): Unit = AG.run { 
    forAll[A ~ (A => B)]("applicative homomorphism") {
      case a ~ f => 
        f.pure[F] <*> a.pure[F] <-> f(a).pure[F]
    }
  }

  @Test def applicativeInterchange(): Unit = AG.run {
    forAll[A ~ F[A => B]]("applicative interchange") {
      case a ~ ff => 
        (ff <*> a.pure[F]) <-> (((f: A => B) => f(a)).pure[F] <*> ff)
    }
  }

  @Test def applicativeMapLaw(): Unit = AG.run {
    forAll[F[A] ~ (A => B)]("ap must be consistent with map") {
        case fa ~ f => (fa map f) <-> (f.pure[F] <*> fa)
    }
  }

  @Test def apProductConsistentLaw(): Unit = AG.run {
    forAll[F[A] ~ F[A => B]]("ap must be consistent with product") {
     case fa ~ f => 
      f <*> fa <-> (f |@| fa).map { case (f, a) => f(a) }
    }
  }

  @Test def apCompositionLaw(): Unit = 
    val compose: (B => C) => (A => B) => (A => C) = _.compose
    AG.run {
      forAll[F[A] ~ F[A => B] ~ F[B => C]]("ap should compose") {
        case fa ~ fab ~ fbc => 
          fbc <*> (fab <*> fa) <-> (fbc.map(compose) <*> fab <*> fa)
      }
    }

  @Test def productRConsistencyLaw(): Unit = AG.run {
    forAll[F[A] ~ F[B]]("productR should behave like product followed by dropping left side with map") {
      case fa ~ fb => 
        (fa *> fb) <-> (fa |@| fb).map((_, b) => b)
    }
  }

  @Test def productLConsistencyLaw(): Unit = AG.run {
    forAll[F[A] ~ F[B]]("productR should behave like product followed by droppig right side with map") {
      case fa ~ fb => 
        (fa <* fb) <-> (fa |@| fb).map((a, _) => a)
    }
  }

object ApplicativeLaws
  class Givens[F[_], A, B, C](given 
    AF: Applicative[F],
    SFA: Show[F[A]],
    SFB: Show[F[B]],
    SFC: Show[F[C]],
    EFA: Eq[F[A]],
    EFB: Eq[F[B]],
    EFC: Eq[F[C]],
    AA: Arbitrary[A],
    AFA: Arbitrary[F[A]],
    AFB: Arbitrary[F[B]],
    AA2B: Arbitrary[A => B],
    AFA2B: Arbitrary[F[A => B]],
    AFB2C: Arbitrary[F[B => C]]
  ) with
    def run(body: (given 
                    Applicative[F],
                    Functor[F], 
                    Show[F[A]],
                    Show[F[B]],
                    Show[F[C]],
                    Eq[F[A]],
                    Eq[F[B]],
                    Eq[F[C]],
                    Arbitrary[A],
                    Arbitrary[F[A]],
                    Arbitrary[F[B]],
                    Arbitrary[A => B],
                    Arbitrary[F[A => B]],
                    Arbitrary[F[B => C]]
                  ) => Unit): Unit = 
        body.apply(given 
          AF, 
          AF.functor, 
          SFA,
          SFB,
          SFC,
          EFA,
          EFB,
          EFC,
          AA,
          AFA,
          AFB,
          AA2B,
          AFA2B,
          AFB2C
        )
