package sql2json
package types

import Validated.given
import FailFastValidated.given
import cat.Applicative.given
import cat.ApplicativeError.given
import cat.Functor.given
import cat.Semigroup.given
import cat.Monad.given
import cat.MonadError.given
import org.junit.Test
import org.junit.Assert._

import testing.laws.EqLaws

class ValidatedTest
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

  @Test def testEqLaws(): Unit = 
    EqLaws.runWith[Validated[Boolean]](Vector(
      true.valid, 
      false.valid, 
      "low".invalid, 
      "high".invalid
    ))
    
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

  @Test def testFunctor(): Unit = 
    assertEquals(
      2.valid.map(_ + 1),
      3.valid
    )
    assertEquals(
      "a".invalid[Int].map(_ + 1),
      "a".invalid
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

  @Test def testFailFastMonadError(): Unit = 
    assertEquals(
      "a".valid.failFast.ensure(NonEmptyList.one("oops"))(_ == "a"),
      "a".valid.failFast
    )
    assertEquals(
      "a".valid.failFast.ensure(NonEmptyList.one("oops"))(_ != "a"),
      "oops".invalid.failFast
    )
    assertEquals(
      "a".valid.failFast.ensureOr(NonEmptyList.one(_))(_ == "a"),
      "a".valid.failFast
    )
    assertEquals(
      "a".valid.failFast.ensureOr(NonEmptyList.one(_))(_ != "a"),
      "a".invalid.failFast
    )