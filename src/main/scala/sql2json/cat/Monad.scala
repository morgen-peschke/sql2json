package sql2json
package cat

trait Monad[C[_]](given val applicative: Applicative[C])
  def flatMap[A,B](ca: C[A], fc: A => C[B]): C[B]

trait MonadErrorProvidesMonad
  given[F[_]](given ME: MonadError[F, ?]): Monad[F] = ME.monad

object Monad extends MonadErrorProvidesMonad
  given ops[C[_],A]: AnyRef
    def[B] (ca: C[A]) flatMap (fc: A => C[B])(given M: Monad[C]): C[B] = M.flatMap(ca, fc)

    def[B] (ca: C[A]) >=> (fc: A => C[B])(given M: Monad[C]): C[B] = M.flatMap(ca, fc)

  given Monad[List]
    def flatMap[A,B](ca: List[A], fc: A => List[B]): List[B] = ca.flatMap(fc)