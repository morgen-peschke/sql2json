package sql2json
package cat

trait Monoid[A] extends Semigroup[A]
  def empty: A

object Monoid
  def apply[A](given M: Monoid[A]): Monoid[A] = M

  def instance[A](zero: A, combineF: (A, A) => A): Monoid[A] = 
    new Monoid[A] with
      def empty: A = zero

      def combine(a: A, b: A): A = combineF(a,b)  

  def empty[A: Monoid]: A = Monoid[A].empty