package sql2json
package testing
package laws

import cat.{Applicative, Functor, Monad, Show, Eq}
import cat.Applicative.{~, given}
import cat.Functor.given
import cat.Monad.given
import testing.Arbitrary
import testing.Arbitrary.forAll
import testing.Result.given
import org.junit.Test

abstract class MonadLaws[F[_], A, B](given MG: MonadLaws.Givens[F, A, B])

  @Test def monadLeftIdentity(): Unit = MG.run {
    forAll[A ~ (A => F[B])]("monad left identity") {
      case a ~ f => 
        a.pure[F].flatMap(f) <-> f(a)
    }
  }

  @Test def monadRightIdentity(): Unit = MG.run {
    forAll[F[A]]("monad right identity") { fa => 
      fa.flatMap(_.pure[F]) <-> fa
    }
  }

  @Test def mapFlatMapCoherence(): Unit = MG.run {
    forAll[F[A] ~ (A => B)]("monad flatMap is coherent with map") {
      case fa ~ f => 
        fa.flatMap(a => f(a).pure[F]) <-> fa.map(f)
    }
  }

object MonadLaws
  class Givens[F[_], A, B](
    given
      MF: Monad[F],
      EFA: Eq[F[A]],
      SFA: Show[F[A]],
      EFB: Eq[F[B]],
      SFB: Show[F[B]],
      AA: Arbitrary[A],
      AFA: Arbitrary[F[A]],
      AA2B: Arbitrary[A => B],
      AA2FB: Arbitrary[A => F[B]]
  ) with
    def run(body: (
      given 
        Monad[F],
        Applicative[F],
        Functor[F],
        Eq[F[A]],
        Show[F[A]],
        Eq[F[B]],
        Show[F[B]],
        Arbitrary[A],
        Arbitrary[F[A]],
        Arbitrary[A => B],
        Arbitrary[A => F[B]]
    ) => Unit): Unit = 
      body.apply(
        given 
          MF,
          MF.applicative,
          MF.applicative.functor,
          EFA,
          SFA,
          EFB,
          SFB,
          AA,
          AFA,
          AA2B,
          AA2FB
      )