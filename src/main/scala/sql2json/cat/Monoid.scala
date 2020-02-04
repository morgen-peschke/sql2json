package sql2json
package cat

trait Monoid[A](given val semigroup: Semigroup[A], E: Empty[A])
  def empty: A = E.empty

trait EmptyAndSemigroupIsMonoid
  given[A: Semigroup: Empty]: Monoid[A] = Monoid.instance

trait MonoidKCanProvideMonoid extends EmptyAndSemigroupIsMonoid
  given[M[_], A] (given M: MonoidK[M]): Monoid[M[A]] = M.monoid[A]

object Monoid extends MonoidKCanProvideMonoid
  def instance[A: Semigroup: Empty]: Monoid[A] =
    new Monoid[A] {}

  def empty[A](given M: Monoid[A]): A = M.empty