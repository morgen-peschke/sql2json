package sql2json
package cat

trait Show[-A]
  def show(a: A): String

object Show extends macros.ShowMacros
  def[A] (a: A) show(given S: Show[A]): String = S.show(a)

  given Show[Nothing] = _ => ??? // Should never be used, but needed for stuff like Nil.show to compile
  given Show[String] = str => s""""$str""""
  given Show[Int] = _.toString
  given Show[Long] = l => s"${l}L"
  given Show[Boolean] = _.toString

  given [L: Show]: Show[Left[L, ?]] = left => show"Left(${left.value})"
  given [R: Show]: Show[Right[?, R]] = right => show"Right(${right.value})"
  given [L: Show, R: Show]: Show[Either[L,R]] = 
    _ match
      case Left(l) => show"Left($l)"
      case Right(r) => show"Right($r)"

  given [A: Show]: Show[List[A]] = _.map(_.show).mkString("[", ",", "]")

  