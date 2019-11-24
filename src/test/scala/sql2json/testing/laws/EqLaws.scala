package sql2json
package testing
package laws

import cat.Eq
import cat.Eq.given
import cat.Show
import cat.Show.given
import testing.Test.{Passed, Failed, given}
import org.junit.Assert.assertThat

object EqLaws

  def reflexitivityEq[A: Show: Eq](x: A): Unit =
    "reflexivity".check(x <-> x)

  def symmetryEq[A: Show: Eq](x: A, y: A): Unit =
    "symmetry" using("x" -> x, "y" -> y) {
      (x === y) <-> (y === x)
    }

  def transitivityEq[A: Show: Eq](x: A, y: A, z: A): Unit =
    "transitivity" using("x" -> x, "y" -> y, "z" -> z) {
      if x =!= y || y =!= z then Passed("x =!= y || y =!= z")
      else if (x === y) && (y === z) then 
        if x === z then Passed("x === y && y === z && x === z")
        else Failed(s"x === y and y === z, but x =!= z")
      else Failed(s"unexpected result") 
    }
  
  def runWith[A: Show: Eq](elements: Seq[A]): Unit = 
    elements.foreach(reflexitivityEq(_))
    
    for 
      x <- elements
      y <- elements
    yield symmetryEq(x, y)

    for 
      x <- elements
      y <- elements
      z <- elements
    yield transitivityEq(x, y, z)