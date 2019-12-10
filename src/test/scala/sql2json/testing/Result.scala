package sql2json
package testing

import org.hamcrest.Matcher
import org.hamcrest.BaseMatcher
import org.hamcrest.Description
import scala.reflect.ClassTag

import cat.Eq
import cat.Eq.given
import cat.Show
import cat.Show.show

/**
 * An attempt to abstract away some of the JUnit boilerplate.
 */
enum Result
  case Passed(description: String)
  case Failed(description: String)

object Result
  /**
   * Helper to combine two results
   */
  def both(a: Result, b: Result): Result = 
    (a, b) match 
      case (Passed(aDesc), Passed(bDesc)) => Passed(s"($aDesc) and ($bDesc)")
      case (Passed(aDesc), Failed(bDesc)) => Failed(s"($aDesc) but ($bDesc)")
      case (Failed(aDesc), Passed(bDesc)) => Failed(s"($bDesc) but ($aDesc)")
      case (Failed(aDesc), Failed(bDesc)) => Failed(s"($aDesc) and ($bDesc)")

  trait ResultOps
    /**
     * Compares two values, saving off a representation of this comparison so the [[Matcher]] implementation
     * doesn't have to duplicate stuff like type checks and calculations for both comparison and description. 
     */
    def[A: Show: Eq] (a: A) <-> (b: A): Result = 
      if a === b
      then Passed(s"${a.show} === ${b.show}")
      else Failed(s"${a.show} =!= ${b.show}")
    
    /**
     * Add a prefix to the test description.
     *
     * Note: does not add a delimiter between the output and description
     */
    def (result: Result) withClue (prefix: String): Result = 
      result match
        case Passed(desc) => Passed(s"$prefix$desc")
        case Failed(desc) => Failed(s"$prefix$desc")

    /**
     * Alternate form of `withClue`, that's sometimes easier to use
     */
    def (prefix: String) asClue (result: Result): Result = 
      result withClue prefix

  given syntax: ResultOps

/**
 * Part of the JUnit [[Matcher]] API that I've always found awkward is the need to
 * either duplicate the calculation of results, or have shared state between 
 * [[Matcher#matches]] and [[Matcher#describeMismatch]].
 *
 * One workaround is to precompute the results and simply asserting that it passed,
 * and using the description it provides.
 */
object Passes extends BaseMatcher[Result]
  override def matches(obj: Any): Boolean = 
    obj match 
      case Result.Passed(_) => true
      case _ => false

  override def describeMismatch(obj: Any, descr: Description): Unit =
    obj match 
      case Result.Passed("") => descr.appendText("passed")
      case Result.Passed(description) => descr.appendText("passed: ").appendText(description)
      case Result.Failed("") => descr.appendText("failed")
      case Result.Failed(msg) => descr.appendText("failed: ").appendText(msg)
      case _ => descr.appendText("[not a Test: ").appendValue(obj).appendText("]")

  override def describeTo(description: Description): Unit = 
    description.appendText("test should pass")
