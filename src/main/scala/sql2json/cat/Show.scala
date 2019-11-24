package sql2json
package cat

trait Show[A]
  def show(a: A): String

object Show
  def apply[A](given S: Show[A]) = S

  trait ShowOps[A]
    def (a: A) show(given S: Show[A]): String = S.show(a)

  given syntax[A]: ShowOps[A]

  given Show[String] = str => s""""$str""""
  given Show[Int] = _.toString
  given Show[Boolean] = _.toString

  // TODO: Figure out how to tranlate the show"" interpolator
