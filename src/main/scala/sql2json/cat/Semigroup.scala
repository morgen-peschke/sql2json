package sql2json
package cat

trait Semigroup[A]
  def combine (a: A, b: A): A

trait SemigroupProviders
  given[C[_], A] (given S: SemigroupK[C]): Semigroup[C[A]] = S.semigroup[A]

  given[A] (given M: Monoid[A]): Semigroup[A] = M.semigroup

object Semigroup extends SemigroupProviders
  given ops[A]: AnyRef
    def (a: A) combine (b: A)(given S: Semigroup[A]): A = S.combine(a,b)

  given forInt: Semigroup[Int] = _ + _
  given forLong: Semigroup[Long] = _ + _
