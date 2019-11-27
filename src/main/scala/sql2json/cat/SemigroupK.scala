package sql2json
package cat

trait SemigroupK[C[_]]
  def combineK[A] (a: C[A], b: C[A]): C[A]

  def semigroup[A]: Semigroup[C[A]] = 
    new Semigroup[C[A]] with
      def combine (a: C[A], b: C[A]): C[A] = combineK(a,b)

trait SemigroupKProviders
  given[C[_]] (given M: MonoidK[C]): SemigroupK[C] = M.semigroupK

object SemigroupK extends SemigroupKProviders
  trait SemigroupKOps[C[_],A]
    def (a: C[A]) combineK (b: C[A])(given SK: SemigroupK[C]): C[A] = SK.combineK(a,b)

  given[C[_],A]: SemigroupKOps[C,A]

  given SemigroupK[List]
    def combineK[A] (a: List[A], b: List[A]): List[A] = a ::: b
