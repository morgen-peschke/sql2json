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
import testing.laws.EqLaws
import testing.laws.FunctorLaws
import testing.Arbitrary

import ValidatedTest.given

final class ValidatedEqLaws extends EqLaws[Validated[Boolean]]

final class ValidatedFunctorLaws extends FunctorLaws[Validated, Int, String, Long]

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
      "b".invalid combine "c".invalid
    )

  @Test def testApplicative(): Unit = 
    assertEquals(
      1.pure[Validated],
      1.valid
    )

  @Test def testApplicativeError(): Unit = 
    assertEquals(
      NonEmptyList.one("a").raise[Validated,Int],
      "a".invalid[Int]
    )

  @Test def testFailFastMonad(): Unit = 
    assertEquals(
      "a".valid.failFast.flatMap(s => s"|$s|".invalid.failFast),
      "|a|".invalid.failFast
    )
    assertEquals(
      "a".valid.failFast.flatMap(s => s"|$s|".valid.failFast),
      "|a|".valid.failFast
    )
    assertEquals(
      "a".invalid.failFast.flatMap(s => s"|$s|".invalid.failFast),
      "a".invalid.failFast
    )
    assertEquals(
      "a".invalid.failFast.flatMap(s => s"|$s|".valid.failFast),
      "a".invalid.failFast
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

  given EqLaws.Givens[Validated[Boolean]] = EqLaws.Givens[Validated[Boolean]]

  given FunctorLaws.Givens[Validated, Int, String, Long] = FunctorLaws.Givens.derive[Validated, Int, String, Long]