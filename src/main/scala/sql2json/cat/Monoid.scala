package sql2json
package cat

trait Monoid[A](given val semigroup: Semigroup[A])
  def empty: A

trait MonoidKCanProvideMonoid
  given[M[_], A] (given M: MonoidK[M]): Monoid[M[A]] = M.monoid[A]

object Monoid extends MonoidKCanProvideMonoid
  def instance[A: Semigroup](zero: A): Monoid[A] =
    new Monoid[A] with
      def empty: A = zero

  def empty[A](given M: Monoid[A]): A = M.empty

  given Monoid[Long] = instance[Long](0L)