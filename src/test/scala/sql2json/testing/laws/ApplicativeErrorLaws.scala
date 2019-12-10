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
import org.junit.Assert.fail
import types.Convertible


abstract class ApplicativeErrorLaws[F[_], E, A, B](given AE: ApplicativeErrorLaws.Givens[F, E, A, B])
  @Test def toAndFromEitherConsistency(): Unit = AE.run {
    forAll[Either[E,A]]("applicativeError toEither/fromEither consistency (fix this first)") { either =>
      either.liftToError[F].toEither <-> either
    }
  }

  @Test def raiseConsistencyWithFromEither(): Unit = AE.run {
    forAll[E]("raise consistency with fromEither on a Left") { e =>
      e.raise[F,A] <-> Left(e).liftToError[F]
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

  @Test def testCatchOnlyReturningValue(): Unit = AE.run {
    forAll[A]("catchOnly returning value consistent with pure") { value =>
      given Convertible[IllegalArgumentException, E] = iae => 
        fail(s"Should have returned value, instead caught $iae")
        ???
      summon[ApplicativeError[F, E]].catchOnly[IllegalArgumentException](value) <-> value.pure[F]
    }
  }

  @Test def testCatchOnlyThrowsExpectedException(): Unit = AE.run {
    forAll[String ~ (String => E)]("catchOnly handling exception consistent with raise") { 
      case exceptionMsg ~ msgToErrorFn =>
        given Convertible[IllegalArgumentException, E] = iae => msgToErrorFn(iae.getMessage)
        summon[ApplicativeError[F, E]].catchOnly[IllegalArgumentException] {
          throw new IllegalArgumentException(exceptionMsg)
        } <-> msgToErrorFn(exceptionMsg).raise[F, A]
    }
  }
   
  @Test def testCatchOnlyThrowsUnexpectedException(): Unit = AE.run {
    forAll[String]("catchOnly propagates exceptions of unexpected types") { exceptionMsg =>
        given Convertible[ClassCastException, E] = 
          iae => 
            fail(s"Should have propagated exception, but caught $iae instead")
            ???
        try
          val result = summon[ApplicativeError[F, E]].catchOnly[ClassCastException] {
            throw new IllegalArgumentException(exceptionMsg)
          }
          fail(s"Should have thrown exception, instead returned $result")
          ???
        catch
          case e: IllegalArgumentException => 
            e.getMessage <-> exceptionMsg
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
      Eq[String],
      Show[String],
      Arbitrary[F[A]],
      Arbitrary[A],
      Arbitrary[E],
      Arbitrary[A => B],
      Arbitrary[E => A],
      Arbitrary[String],
      Arbitrary[String => E],
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
      Eq[String],
      Show[String],
      Arbitrary[F[A]],
      Arbitrary[A],
      Arbitrary[E],
      Arbitrary[A => B],
      Arbitrary[E => A],
      Arbitrary[String],
      Arbitrary[String => E],
      Arbitrary[E => E],
      Arbitrary[E => B],
      Arbitrary[Either[E, A]]
    ) => Unit): Unit = body.apply

  given [F[_], E, A, B](
    given
      ApplicativeError[F,E],
      Eq[Either[E,A]],
      Show[Either[E,A]],
      Eq[F[A]],
      Show[F[A]],
      Eq[B],
      Show[B],
      Eq[String],
      Show[String],
      Arbitrary[F[A]],
      Arbitrary[A],
      Arbitrary[E],
      Arbitrary[A => B],
      Arbitrary[E => A],
      Arbitrary[String],
      Arbitrary[String => E],
      Arbitrary[E => E],
      Arbitrary[E => B],
      Arbitrary[Either[E, A]]
  ): Givens[F, E, A, B]