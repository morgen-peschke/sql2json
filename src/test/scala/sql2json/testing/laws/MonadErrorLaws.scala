package sql2json
package testing
package laws

import cat.{ApplicativeError, Eq, Show, Functor, Monad, MonadError}
import cat.ApplicativeError.given
import cat.Functor.given
import cat.Monad.given
import cat.MonadError.given
import cat.Applicative.{~, given}
import testing.Arbitrary
import testing.Arbitrary.forAll
import testing.Result.given
import org.junit.Test

abstract class MonadErrorLaws[F[_], E, A, B](given ME: MonadErrorLaws.Givens[F, E, A, B])

  @Test def monadErrorLeftZero(): Unit = ME.run {
    forAll[E ~ (A => F[B])]("monadError left zero") {
      case e ~ f => 
        e.raise[F, A].flatMap(f) <-> e.raise[F, B]
    }
  }

  @Test def monadErrorEnsureConsistency(): Unit = ME.run {
    forAll[F[A] ~ E ~ (A => Boolean)]("monadError ensure consistency") {
      case fa ~ e ~ p =>
        fa.ensure(e)(p) <-> fa.flatMap(a => if (p(a)) a.pure[F] else e.raise[F,A])
    }
  }

  @Test def monadErrorEnsureOrConsistency(): Unit = ME.run {
    forAll[F[A] ~ (A => E) ~ (A => Boolean)]("monadError ensureOr consistency") {
      case fa ~ ae ~ p =>
      fa.ensureOr(ae)(p) <-> fa.flatMap(a => if (p(a)) a.pure[F] else ae(a).raise[F,A])
    }
  }

object MonadErrorLaws
  class Givens[F[_], E, A, B](
    given
      MonadError[F,E],
      Eq[F[A]],
      Show[F[A]],
      Eq[F[B]],
      Show[F[B]],
      Arbitrary[E],
      Arbitrary[F[A]],
      Arbitrary[A => E],
      Arbitrary[E => E],
      Arbitrary[A => Boolean], 
      Arbitrary[A => F[B]]
  ) with
    def run(body: (
      given
        MonadError[F, E],
        Eq[F[A]],
        Show[F[A]],
        Eq[F[B]],
        Show[F[B]],
        Arbitrary[E],
        Arbitrary[F[A]],
        Arbitrary[A => E],
        Arbitrary[E => E],
        Arbitrary[A => Boolean], 
        Arbitrary[A => F[B]]
      ) => Unit
    ): Unit = 
      body.apply

  given[F[_], E, A, B](
    given
      MonadError[F,E],
      Eq[F[A]],
      Show[F[A]],
      Eq[F[B]],
      Show[F[B]],
      Arbitrary[E],
      Arbitrary[F[A]],
      Arbitrary[A => E],
      Arbitrary[E => E],
      Arbitrary[A => Boolean], 
      Arbitrary[A => F[B]]
  ): Givens[F,E,A,B]