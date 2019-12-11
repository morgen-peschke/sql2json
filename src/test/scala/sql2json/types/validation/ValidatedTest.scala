package sql2json
package types
package validation

import Accumulate.given
import FailFast.given
import cat.{Eq,Show,Functor, ApplicativeError}
import cat.Applicative.{~, given}
import cat.ApplicativeError.given
import cat.Functor.given
import cat.Monad.given
import cat.MonadError.given
import org.junit.Test
import org.junit.Assert._
import testing.laws.{ApplicativeLaws, ApplicativeErrorLaws, EqLaws, FunctorLaws, MonadLaws, MonadErrorLaws, SemigroupLaws}
import testing.{Arbitrary, Gen, Cogen}
import testing.Arbitrary.forAll
import testing.Result.given
import types.ConvertibleK.given

import ValidatedTest.given

final class AccumulateEqLaws extends EqLaws[Accumulate.Validated[Boolean]]
final class FailFastEqLaws extends EqLaws[FailFast.Validated[Boolean]]

final class AccumulateFunctorLaws extends FunctorLaws[Accumulate.Validated, Int, String, Long]
final class FailFastFunctorLaws extends FunctorLaws[FailFast.Validated, Int, String, Long]

final class AccumulateApplicativeLaws extends ApplicativeLaws[Accumulate.Validated, Int, String, Long]
final class FailFastApplicativeLaws extends ApplicativeLaws[FailFast.Validated, Int, String, Long]

final class AccumulateApplicativeErrorLaws extends ApplicativeErrorLaws[Accumulate.Validated, Errors, Int, String]
final class FailFastApplicativeErrorLaws extends ApplicativeErrorLaws[FailFast.Validated, Errors, Int, String]

final class FailFastMonadLaws extends MonadLaws[FailFast.Validated, Int, String]
final class FailFastMonadErrorLaws extends MonadErrorLaws[FailFast.Validated, Errors, Int, String]

final class AccumulateSemigroupLaws extends SemigroupLaws[Accumulate.Validated[Int]]
final class FailFastSemigroupLaws extends SemigroupLaws[FailFast.Validated[Int]]

final class AccumulateTest 
  import Accumulate.Validated
  @Test def validConsistentWithPure(): Unit = 
    forAll[Int]("valid consistency with pure") { value =>
      value.valid <-> value.pure[Validated]
    }
  
  @Test def invalidConsistentWithRaise(): Unit =
    forAll[String]("invalid consistency with raise") { error =>
      error.invalid[Int] <-> NonEmptyList.one(error).raise[Validated, Int]
    }

  @Test def testAsValidatedFromNone(): Unit =
    assertEquals(
      Option.empty[Int].asValidated("Missing").toEither,
      Left(NonEmptyList.one("Missing"))
    )

  @Test def testAsValidatedFromSome(): Unit = 
    assertEquals(
      Some("hi there").asValidated("Missing").toEither,
      Right("hi there")
    )

object ValidatedTest
  given arbAccum[A](given Arbitrary[A]): Arbitrary[Accumulate.Validated[A]] =
    Arbitrary.oneOf(
      Arbitrary[A].map(_.valid),
      Arbitrary[String].map(_.invalid[A])
    )    

  given arbFFast[A](given A: Arbitrary[Accumulate.Validated[A]]): Arbitrary[FailFast.Validated[A]] =
    A.map(_.asKind[FailFast.Validated])

  given [A](given GA: Gen[A], GS: Gen[String]): Gen[FailFast.Validated[A]] = Gen.usingRandom { rng => 
    if (rng.nextBoolean) rng.nextString(rng.nextInt(20)).invalid[A].asKind[FailFast.Validated]
    else GA.fromSeed(rng.nextLong).valid.asKind[FailFast.Validated]
  }

  given [A](given CA: Cogen[A], CE: Cogen[NonEmptyList[String]]): Cogen[FailFast.Validated[A]] = 
    _.toEither match 
      case Left(nel) => 2L + CE.toSeed(nel)
      case Right(a)  => 1L + CA.toSeed(a)