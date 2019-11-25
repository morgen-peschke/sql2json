package sql2json
package cat

trait Monad[C[_]](given val applicative: Applicative[C])
  def flatMap[A,B](ca: C[A], fc: A => C[B]): C[B]

object Monad
  def apply[C[_]](given Monad[C]) = summon[Monad[C]]

  trait MonadOps[C[_],A]
    def[B] (ca: C[A]) flatMap (fc: A => C[B])(given M: Monad[C]): C[B] = M.flatMap(ca, fc)

    def[B] (ca: C[A]) >=> (fc: A => C[B])(given M: Monad[C]): C[B] = M.flatMap(ca, fc)

  given[C[_],A]: MonadOps[C,A]