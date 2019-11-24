package sql2json
package cat

trait Semigroup[A]
  def combine (a: A, b: A): A

object Semigroup
  def apply[A](given S: Semigroup[A]): Semigroup[A] = S

  given fromSemigroupK[C[_], A] (given S: SemigroupK[C]): Semigroup[C[A]] = S.semigroup[A]

  trait SemigroupOps[A]
    def (a: A) combine (b: A)(given S: Semigroup[A]): A = S.combine(a,b)

  given syntax[A]: SemigroupOps[A]

  given Semigroup[Int]
    def combine (a: Int, b: Int): Int = a + b