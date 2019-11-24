package sql2json
package cat

trait MonoidK[C[_]] extends SemigroupK[C]
  def emptyK[A]: C[A] 

  def monoid[A]: Monoid[C[A]] = 
    new Monoid[C[A]] with
      def empty: C[A] = emptyK

      def combine (a: C[A], b: C[A]): C[A] = combineK(a, b)

object MonoidK
  def apply[C[_]](given M: MonoidK[C]): MonoidK[C] = M

  given MonoidK[List]
    def emptyK[A]: List[A] = Nil

    def combineK[A] (a: List[A], b: List[A]): List[A] = a ::: b