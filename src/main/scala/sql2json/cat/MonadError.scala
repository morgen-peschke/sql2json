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

  def prevent[A](fa: C[A], error: => E, predicate: A => Boolean): C[A] = ensure(fa, error, a => !predicate(a))
  def preventOr[A](fa: C[A], error: A => E, predicate: A => Boolean): C[A] = ensureOr(fa, error, a => !predicate(a))

  def catchOnly[T <: Throwable]: ApplicativeError.CatchOnlyPartiallyApplied[C,E,T] = applicativeError.catchOnly[T]

object MonadError
  class DerivedMonadError[C[_], E](given Monad[C], ApplicativeError[C,E]) extends MonadError[C,E]

  def derived[C[_], E](given Monad[C], ApplicativeError[C,E]): MonadError[C, E] = new DerivedMonadError[C,E]

  given ops[C[_],A]: AnyRef
    def[E] (fa: C[A]) ensure (error: => E)(predicate: A => Boolean)(given ME: MonadError[C,E]): C[A] =
      ME.ensure(fa, error, predicate)

    def[E] (fa: C[A]) ensureOr(error: A => E)(predicate: A => Boolean)(given ME: MonadError[C,E]): C[A] =
      ME.ensureOr(fa, error, predicate)

    def[E] (fa: C[A]) prevent (error: => E)(predicate: A => Boolean)(given ME: MonadError[C,E]): C[A] =
      ME.prevent(fa, error, predicate)

    def[E] (fa: C[A]) preventOr(error: A => E)(predicate: A => Boolean)(given ME: MonadError[C,E]): C[A] =
      ME.preventOr(fa, error, predicate)