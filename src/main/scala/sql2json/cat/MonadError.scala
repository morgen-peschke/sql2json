package sql2json
package cat

import Applicative.given
import ApplicativeError.given
import Monad.given

trait MonadError[C[_], E](given val monad: Monad[C], val applicativeError: ApplicativeError[C, E])
  def ensure[A](fa: C[A], error: => E, predicate: A => Boolean): C[A] =
    fa.flatMap(a => if (predicate(a)) a.pure[C] else error.raise)

  def ensureOr[A](fa: C[A], error: A => E, predicate: A => Boolean): C[A] =
    fa.flatMap(a => if (predicate(a)) a.pure[C] else error(a).raise)

object MonadError
  class DerivedMonadError[C[_], E](given Monad[C], ApplicativeError[C,E]) extends MonadError[C,E]

  def derived[C[_], E](given Monad[C], ApplicativeError[C,E]): MonadError[C, E] = new DerivedMonadError[C,E]

  trait MonadErrorOps[C[_],A]
    def[E] (fa: C[A]) ensure (error: => E)(predicate: A => Boolean)(given ME: MonadError[C,E]): C[A] =
      ME.ensure(fa, error, predicate)

    def[E] (fa: C[A]) ensureOr(error: A => E)(predicate: A => Boolean)(given ME: MonadError[C,E]): C[A] =
      ME.ensureOr(fa, error, predicate)

  given[C[_],A]: MonadErrorOps[C,A]