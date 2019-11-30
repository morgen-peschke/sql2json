package sql2json
package testing
package laws

import cat.{ApplicativeError, Eq, Show, Functor, Monad}
import cat.ApplicativeError.given
import cat.Functor.given
import cat.Monad.given
import cat.Applicative.{~, given}
import testing.Arbitrary
import testing.Arbitrary.forAll
import testing.Result.given
import org.junit.Test


abstract class ApplicativeErrorLaws[F[_], E, A, B](given AE: ApplicativeErrorLaws.Givens[F, E, A, B])
  @Test def toAndFromEitherConsistency(): Unit = AE.run {
    forAll[Either[E,A]]("applicativeError toEither/fromEither consistency (fix this first)") { either =>
      ApplicativeError.fromEither[F](either).toEither <-> either
    }
  }

  @Test def raiseConsistencyWithFromEither(): Unit = AE.run {
    forAll[E]("raise consistency with fromEither on a Left") { e =>
      e.raise[F,A] <-> ApplicativeError.fromEither[F](Left(e))
    }
  }

  @Test def recoverConsistentWithPure(): Unit = AE.run {
    forAll[E ~ (E => A)]("recover consistency with pure") {
      case e ~ f =>
        e.raise[F,A].recover(f) <-> f(e).pure[F]
    }
  }

  @Test def mapErrorIdentity(): Unit = AE.run {
    forAll[F[A]]("mapError identity") { fa => 
      fa.mapError[E](identity) <-> fa
    }
  }

  @Test def mapErrorConsistencyWithRaise(): Unit = AE.run {
    forAll[E ~ (E => E)]("mapError consistency with raise") { 
      case e ~ f => 
        e.raise[F,A].mapError[E](f) <-> f(e).raise[F,A]
    }
  }

  @Test def foldCompositionForErrors(): Unit = AE.run {
    forAll[E ~ (E => B) ~ (A => B)]("fold composes for errors") {
      case e ~ fe ~ fa =>
        e.raise[F,A].fold(fe, fa) <-> fe(e)
    }
  }

  @Test def foldCompositionForSuccesses(): Unit = AE.run {
    forAll[A ~ (E => B) ~ (A => B)]("fold composes for errors") {
      case a ~ fe ~ fa =>
        a.pure[F].fold(fe, fa) <-> fa(a)
    }
  }

object ApplicativeErrorLaws
  class Givens[F[_], E, A, B](
    given
      ApplicativeError[F,E],
      Eq[Either[E,A]],
      Show[Either[E,A]],
      Eq[F[A]],
      Show[F[A]],
      Eq[B],
      Show[B],
      Arbitrary[F[A]],
      Arbitrary[A],
      Arbitrary[E],
      Arbitrary[A => B],
      Arbitrary[E => A],
      Arbitrary[E => E],
      Arbitrary[E => B],
      Arbitrary[Either[E, A]]
  ) with
    def run(body: (given 
      ApplicativeError[F,E],
      Eq[Either[E,A]],
      Show[Either[E,A]],
      Eq[F[A]],
      Show[F[A]],
      Eq[B],
      Show[B],
      Arbitrary[F[A]],
      Arbitrary[A],
      Arbitrary[E],
      Arbitrary[A => B],
      Arbitrary[E => A],
      Arbitrary[E => E],
      Arbitrary[E => B],
      Arbitrary[Either[E, A]]
    ) => Unit): Unit = body.apply