package sql2json
package cat

trait Eq[A]
  def equiv(a: A, b: A): Boolean
  
object Eq
  def apply[A](given E: Eq[A]): Eq[A] = E

  trait EqOps[A]
    def (a: A) === (b: A)(given E: Eq[A]): Boolean = E.equiv(a, b)

    def (a: A) =!= (b: A)(given E: Eq[A]): Boolean = !E.equiv(a, b)

  given[A]: EqOps[A]

  given Eq[String]  = _ == _
  given Eq[Int]     = _ == _
  given Eq[Long]    = _ == _
  given Eq[Boolean] = _ == _