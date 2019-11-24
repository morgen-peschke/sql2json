package sql2json
package cat

trait MonadError[C[_]] extends Monad[C] with ApplicativeError[C]
  type E

  def ensure[A](fa: C[A], error: => E, predicate: A => Boolean): C[A] =
    flatMap(fa, a => if (predicate(a)) pure(a) else raise(error))

  def ensureOr[A](fa: C[A], error: A => E, predicate: A => Boolean): C[A] =
    flatMap(fa, a => if (predicate(a)) pure(a) else raise(error(a)))

object MonadError
  type Aux[C[_], E] = MonadError[C] with
    type E

  def apply[C[_], E](given ME: MonadError.Aux[C,E]) = ME

  trait MonadErrorOps[C[_],A]
    def[E] (fa: C[A]) ensure (error: => E)(predicate: A => Boolean)(given ME: Aux[C,E], ev: E =:= ME.E): C[A] =
      ME.ensure(fa, ev(error), predicate)

    def[E] (fa: C[A]) ensureOr(error: A => E)(predicate: A => Boolean)(given ME: Aux[C,E], ev: E =:= ME.E): C[A] =
      ME.ensureOr(fa, error andThen ev, predicate)

  given syntax[C[_],A]: MonadErrorOps[C,A]