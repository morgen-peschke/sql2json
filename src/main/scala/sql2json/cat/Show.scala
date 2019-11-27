package sql2json
package cat

trait Show[A]
  def show(a: A): String

object Show
  trait ShowOps[A]
    def (a: A) show(given S: Show[A]): String = S.show(a)

  given[A]: ShowOps[A]

  given Show[String] = str => s""""$str""""
  given Show[Int] = _.toString
  given Show[Long] = l => s"${l}L"
  given Show[Boolean] = _.toString
  given [L: Show, R: Show]: Show[Either[L,R]] = 
    _ match
      case Left(l) => s"Left(${l.show})"
      case Right(r) => s"Right(${r.show})"

  // TODO: Figure out how to tranlate the show"" interpolator
