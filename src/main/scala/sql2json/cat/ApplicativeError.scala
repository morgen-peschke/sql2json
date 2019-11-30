package sql2json
package cat

import Applicative.given
import Bifunctor.given

trait ApplicativeError[C[_], E](given val applicative: Applicative[C])
  def raise[A](error: E): C[A]

  def recover[A](ca: C[A], f: E => A): C[A]

  def fold[A, B] (ca: C[A], fe: E => B, fa: A => B): B

  def mapError[A, EO <: E](cae: C[A], fe: E => EO): C[A] =
    fold(cae, e => raise(fe(e)), _.pure)

  def toEither[A](ca: C[A]): Either[E, A] = fold(ca, Left(_), Right(_))
  
trait MonadErrorProvidesApplicativeError
  given [F[_], E] (given ME: MonadError[F, E]): ApplicativeError[F, E] = ME.applicativeError

object ApplicativeError extends MonadErrorProvidesApplicativeError
  def fromEither[F[_]]: FromEitherPartiallyApplied[F] = new FromEitherPartiallyApplied[F]

  class FromEitherPartiallyApplied[F[_]](val ignored: Boolean = true) extends AnyVal
    def apply[A, E](either: Either[E, A])(given AE: ApplicativeError[F, E]): F[A] = 
      either match
        case Left(e) => e.raise[F, A]
        case Right(a) => a.pure[F]

  trait ApplicativeErrorLifts[E]
    def[C[_], A] (error: E) raise (given AE: ApplicativeError[C,E]): C[A] = AE.raise[A](error)

  trait ApplicativeErrorOps[C[_],A]
    def[E] (ca: C[A]) recover (f: E => A)(given AE: ApplicativeError[C,E]): C[A] = AE.recover(ca, f)

    def[B, E] (ca: C[A]) fold(fe: E => B, fa: A => B)(given AE: ApplicativeError[C,E]): B = AE.fold(ca, fe, fa)

    def[E] (cae: C[A]) mapError (fe: E => E)(given AE: ApplicativeError[C,E]): C[A] = AE.mapError[A, E](cae, fe)

    def[E] (cae: C[A]) toEither (given AE: ApplicativeError[C,E]): Either[E, A] = AE.toEither(cae)

  given[E]: ApplicativeErrorLifts[E]
  given[C[_],A]: ApplicativeErrorOps[C,A]