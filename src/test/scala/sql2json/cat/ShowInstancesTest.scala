package sql2json
package cat

import org.junit.Test
import org.junit.Assert._
import cat.Show.given

final class ShowIterpolatorTest {
  @Test def interpolatorShouldCompile(): Unit = 
    assertEquals("3", show"${1 + 2}")
    assertEquals("<<<[1,2,3]>>>", show"<<<${List(1,2,3)}>>>")
    assertEquals(
      """<<<Left("Hi there")-[1,2,3]-Right("Bye now")>>>""", 
      show"<<<${Left("Hi there")}-${List(1,2,3)}-${Right("Bye now")}>>>"
    )

  @Test def listShowShouldLookLikePythonSyntax(): Unit = 
    assertEquals("[]", Nil.show)
    assertEquals("[]", List.empty[Int].show)
    assertEquals("[1]", List(1).show)
    assertEquals("[1,2]", List(1,2).show)

  @Test def eitherShowShouldHandleLeftAndRight(): Unit = 
    assertEquals("Right(3L)", Right(3L).show)
    assertEquals("Left(7)", Left(7).show)
    assertEquals("Right(3L)", (Right(3L): Either[String, Long]).show)
    assertEquals("Left(7)", (Left(7): Either[Int, Long]).show)
}