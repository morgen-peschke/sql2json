package sql2json
package types

import Validated.given
import FailFastValidated.given
import cat.{Eq,Show,Functor}
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

import ValidatedTest.given

final class ValidatedEqLaws extends EqLaws[Validated[Boolean]]
final class FailFastValidatedEqLaws extends EqLaws[FailFastValidated[Boolean]]

final class ValidatedFunctorLaws extends FunctorLaws[Validated, Int, String, Long]
final class FailFastValidatedFunctorLaws extends FunctorLaws[FailFastValidated, Int, String, Long]

final class ValidatedApplicativeLaws extends ApplicativeLaws[Validated, Int, String, Long]
final class FailFastValidatedApplicativeLaws extends ApplicativeLaws[FailFastValidated, Int, String, Long]

final class ValidatedApplicativeErrorLaws extends ApplicativeErrorLaws[Validated, Errors, Int, String]
final class FailFastValidatedApplicativeErrorLaws extends ApplicativeErrorLaws[FailFastValidated, Errors, Int, String]

final class FailFastValidatedMonadLaws extends MonadLaws[FailFastValidated, Int, String]
final class FailFastValidatedMonadErrorLaws extends MonadErrorLaws[FailFastValidated, Errors, Int, String]

final class ValidatedSemigroupLaws extends SemigroupLaws[Validated[Int]]

final class ValidatedTest 
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
    
  @Test def testCatchOnlyReturningValue(): Unit = 
    assertEquals(
      Validated.catchOnly[IllegalArgumentException] {
        "returned from body"
      }.toEither,
      Right("returned from body")
    )

  @Test def testCatchOnlyThrowsExpectedException(): Unit = 
    assertEquals(
      Validated.catchOnly[IllegalArgumentException] {
        throw new IllegalArgumentException("Oops!")
      }.toEither,
      Left(NonEmptyList.one("java.lang.IllegalArgumentException: Oops!"))
    )

  @Test def testCatchOnlyThrowsUnexpectedException(): Unit = 
    try
      val result = Validated.catchOnly[IllegalArgumentException] {
        throw new IllegalStateException("Oops!")
      }
      fail(s"Should have thrown exception, instead returned $result")
    catch {
      case e: Throwable => 
        assertEquals(
          e.toString,
          "java.lang.IllegalStateException: Oops!"
        )
    }

object ValidatedTest
  given [A](given Arbitrary[A]): Arbitrary[Validated[A]] =
    Arbitrary.oneOf(
      Arbitrary[A].map(_.valid),
      Arbitrary[String].map(_.invalid[A])
    )    

  given [A](given A: Arbitrary[Validated[A]]): Arbitrary[FailFastValidated[A]] =
    A.map(_.failFast)

  given [A](given GA: Gen[A], GS: Gen[String]): Gen[FailFastValidated[A]] = Gen.usingRandom { rng => 
    if (rng.nextBoolean) rng.nextString(rng.nextInt(20)).invalid[A].failFast
    else GA.fromSeed(rng.nextLong).valid.failFast
  }

  given [A](given CA: Cogen[A], CE: Cogen[NonEmptyList[String]]): Cogen[FailFastValidated[A]] = 
    _.toEither match 
      case Left(nel) => 2L + CE.toSeed(nel)
      case Right(a)  => 1L + CA.toSeed(a)