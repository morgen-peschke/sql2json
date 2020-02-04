package sql2json
package cat

trait MonoidK[C[_]](given val semigroupK: SemigroupK[C], EK: EmptyK[C])
  def emptyK[A]: C[A] = EK.emptyK[A]

  def monoid[A]: Monoid[C[A]] = 
    Monoid.instance[C[A]](given semigroupK.semigroup, EK.empty)

object MonoidK
  given MonoidK[List]
    def combineK[A] (a: List[A], b: List[A]): List[A] = a ::: b