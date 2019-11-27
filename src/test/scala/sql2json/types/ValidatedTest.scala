package sql2json
package types

import Validated.given
import FailFastValidated.given
import cat.{Eq,Show,Functor}
import cat.Applicative.given
import cat.ApplicativeError.given
import cat.Functor.given
import cat.Semigroup.given
import cat.Monad.given
import cat.MonadError.given
import org.junit.Test
import org.junit.Assert._
import testing.laws.{ApplicativeLaws, EqLaws, FunctorLaws, MonadLaws}
import testing.{Arbitrary, Gen, Cogen}

import ValidatedTest.given

final class ValidatedEqLaws extends EqLaws[Validated[Boolean]]
final class FailFastValidatedEqLaws extends EqLaws[FailFastValidated[Boolean]]

final class ValidatedFunctorLaws extends FunctorLaws[Validated, Int, String, Long]
final class FailFastValidatedFunctorLaws extends FunctorLaws[FailFastValidated, Int, String, Long]

final class ValidatedApplicativeLaws extends ApplicativeLaws[Validated, Int, String, Long]
final class FailFastValidatedApplicativeLaws extends ApplicativeLaws[FailFastValidated, Int, String, Long]

final class FailFastValidatedMonadLaws extends MonadLaws[FailFastValidated, Int, String]

final class ValidatedTest 

  @Test def testToEitherRight(): Unit = 
    assertEquals(
      true.valid.toEither,
      Right(true)
    )

  @Test def testToEitherLeft(): Unit = 
    assertEquals(
      "hi there".invalid.toEither,
      Left(NonEmptyList.one("hi there"))
    )

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

  @Test def testSemigroup(): Unit = 
    assertEquals(
      1.valid combine 2.valid,
      3.valid
    )
    assertEquals(
      "b".invalid[Int],
      1.valid combine "b".invalid
    )
    assertEquals(
      "a".invalid[Int],
      "a".invalid[Int] combine 2.valid
    )
    assertEquals(
      NonEmptyList.of("b", "c").invalid[Int],
      "b".invalid[Int] combine "c".invalid[Int]
    )

  @Test def testApplicativeError(): Unit = 
    assertEquals(
      NonEmptyList.one("a").raise[Validated,Int],
      "a".invalid[Int]
    )

  // @Test def testFailFastMonadError(): Unit = 
  //   assertEquals(
  //     "a".valid.failFast.ensure[Errors](NonEmptyList.one("oops"))(_ == "a"),
  //     "a".valid.failFast
  //   )
  //   assertEquals(
  //     "a".valid.failFast.ensure[Errors](NonEmptyList.one("oops"))(_ != "a"),
  //     "oops".invalid.failFast
  //   )
  //   assertEquals(
  //     "a".valid.failFast.ensureOr[Errors](NonEmptyList.one(_))(_ == "a"),
  //     "a".valid.failFast
  //   )
  //   assertEquals(
  //     "a".valid.failFast.ensureOr[Errors](NonEmptyList.one(_))(_ != "a"),
  //     "a".invalid.failFast
  //   )  


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

  given eqGivens: EqLaws.Givens[Validated[Boolean]] = EqLaws.Givens[Validated[Boolean]]
  given eqGivensFF: EqLaws.Givens[FailFastValidated[Boolean]] = EqLaws.Givens[FailFastValidated[Boolean]]

  given functorGivens: FunctorLaws.Givens[Validated, Int, String, Long] = FunctorLaws.Givens[Validated, Int, String, Long]
  given functorGivensFF: FunctorLaws.Givens[FailFastValidated, Int, String, Long] = FunctorLaws.Givens[FailFastValidated, Int, String, Long]

  given applicativeGivens: ApplicativeLaws.Givens[Validated, Int, String, Long] = ApplicativeLaws.Givens[Validated, Int, String, Long]
  given applicativeGivensFF: ApplicativeLaws.Givens[FailFastValidated, Int, String, Long] = ApplicativeLaws.Givens[FailFastValidated, Int, String, Long]

  given monadGivensFF: MonadLaws.Givens[FailFastValidated, Int, String] = MonadLaws.Givens[FailFastValidated, Int, String]