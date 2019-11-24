package sql2json
package testing

import org.junit.Assert.assertThat
import org.hamcrest.Matcher
import org.hamcrest.BaseMatcher
import org.hamcrest.Description
import scala.reflect.ClassTag

import cat.Eq
import cat.Eq.given
import cat.Show
import cat.Show.given

/**
 * An attempt to abstract away some of the JUnit boilerplate.
 * 
 * It's currently doing something weird, where the code is expecting [[Int]] rather 
 * than [[Test]]:
 * {{{
   [error] -- [E007] Type Mismatch Error: .../sql2json/src/test/scala/sql2json/testing/laws/EqLaws.scala:20:6
   [error] 20 |      (x === y) <-> (y === x)
   [error]    |      ^^^^^^^^^^^^^^^^^^^^^^
   [error]    |      Found:    sql2json.testing.Test
   [error]    |      Required: Int
 }}}
 */
enum Test
  case Passed(description: String)
  case Failed(description: String)

object Test
  trait TestOps
    def[A: Show: Eq] (a: A) <-> (b: A): Test = 
      if a === b
      then Test.Passed(s"${a.show} === ${b.show}")
      else Test.Failed(s"${a.show} =!= ${b.show}")

    def (test: Test) withClue (prefix: String): Test = 
      test match
        case Passed(desc) => Passed(s"$prefix$desc")
        case Failed(desc) => Failed(s"$prefix$desc")

    def[A: Show] (name: String) using (a0: (String, A), aN: (String, A)*)(test: Test) = 
      val aDesc = (a0 :: aN.toList).map((n, v) => s"$n=${v.show}").mkString("{", ", ", "}")
      name check (test withClue s"Using $aDesc")
    
    def (name: String) check (test: Test): Unit = 
      assertThat(test withClue s"[$name]", Passes)

  given syntax: TestOps

object Passes extends BaseMatcher[Test]
  override def matches(obj: Any): Boolean = 
    obj match 
      case Test.Passed => true
      case _ => false

  override def describeMismatch(obj: Any, descr: Description): Unit =
    obj match 
      case Test.Passed("") => descr.appendText("passed")
      case Test.Passed(description) => descr.appendText("passed: ").appendText(description)
      case Test.Failed("") => descr.appendText("failed")
      case Test.Failed(msg) => descr.appendText("failed: ").appendText(msg)
      case _ => descr.appendText("[not a Test: ").appendValue(obj).appendText("]")

  override def describeTo(description: Description): Unit = 
    description.appendText("test should pass")
