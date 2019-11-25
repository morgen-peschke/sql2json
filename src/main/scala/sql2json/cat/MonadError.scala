package sql2json
package cat

import Applicative.given
import ApplicativeError.given
import Monad.given

trait MonadError[C[_]](given val M: Monad[C], val AE: ApplicativeError[C])
  type E = AE.E

  private given Applicative[C] = M.applicative

  def ensure[A](fa: C[A], error: => E, predicate: A => Boolean): C[A] =
    fa.flatMap(a => if (predicate(a)) a.pure[C] else error.raise)

  def ensureOr[A](fa: C[A], error: A => E, predicate: A => Boolean): C[A] =
    fa.flatMap(a => if (predicate(a)) a.pure else error(a).raise)

object MonadError
  type Aux[C[_], E] = MonadError[C] with
    type E

  def apply[C[_], E](given ME: MonadError.Aux[C,E]) = ME

  def derived[C[_], E](given M: Monad[C], AE: ApplicativeError.Aux[C,E]): MonadError.Aux[C, E] = 
    new MonadError[C](given M, AE) with
      type E = AE.E

  trait MonadErrorOps[C[_],A](given val ME: MonadError[C])
    def[E] (fa: C[A]) ensure (error: => E)(predicate: A => Boolean)(given ME: Aux[C,E], ev: E <:< ME.E): C[A] =
      ME.ensure(fa, ev(error), predicate)

    def[E] (fa: C[A]) ensureOr(error: A => E)(predicate: A => Boolean)(given ME: Aux[C,E], ev: E <:< ME.E): C[A] =
      ME.ensureOr(fa, error andThen ev, predicate)

  given[C[_],A](given MonadError[C]): MonadErrorOps[C,A]