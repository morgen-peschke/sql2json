package sql2json
package cat

trait Semigroup[A]
  def combine (a: A, b: A): A

trait SemigroupKCanProvideSemigroup
  given[C[_], A] (given S: SemigroupK[C]): Semigroup[C[A]] = S.semigroup[A]

object Semigroup extends SemigroupKCanProvideSemigroup
  def apply[A](given S: Semigroup[A]): Semigroup[A] = S

  trait SemigroupOps[A]
    def (a: A) combine (b: A)(given S: Semigroup[A]): A = S.combine(a,b)

  given[A]: SemigroupOps[A]

  given Semigroup[Int] = _ + _
