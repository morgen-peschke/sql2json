package sql2json
package cat

trait MonoidK[C[_]](given val semigroupK: SemigroupK[C])
  def emptyK[A]: C[A] 

  def monoid[A]: Monoid[C[A]] = 
    new Monoid[C[A]](given semigroupK.semigroup) with
      def empty: C[A] = emptyK

  def withSemigroupK[B](body: (given SemigroupK[C]) => B): B = body(given semigroupK)

object MonoidK
  given MonoidK[List]
    def emptyK[A]: List[A] = Nil

    def combineK[A] (a: List[A], b: List[A]): List[A] = a ::: b