package sql2json
package types
package validation

import cat.{Show, Functor, Eq, Applicative, ApplicativeError, Semigroup, Monad, MonadError}
import cat.Applicative.given
import cat.ApplicativeError.given
import cat.MonadError.given
import cat.Functor.given
import cat.Show.show
import cat.Eq.given
import cat.Semigroup.given
import cat.SemigroupK.given
import Validated.{Valid, Invalid, given}

/**
 * An attempt to stay zero-cost, and introduce a way to tell at the type level if the 
 * underlying [[Either]] is fail-fast or accumulating.
 */
 opaque type FailFastValidated[A] = Either[Errors, A]
 object FailFastValidated
  private def[A](ffv: FailFastValidated[A]) unwrapped (given V: ApplicativeError[Validated, Errors]): Validated[A] = 
    V.fromEither(ffv)

  private def[A](v: Validated[A]) wrapped (given V: ApplicativeError[Validated, Errors]): FailFastValidated[A] = 
    v.toEither

  trait FailFastOps[A]
    def (ffva: FailFastValidated[A]) accumulate: Validated[A] = ffva.unwrapped
    def (va: Validated[A]) failFast: FailFastValidated[A] = va.wrapped

  given[A]: FailFastOps[A]

  given[A](given Show[Validated[A]]): Show[FailFastValidated[A]] = ffv => show"FailFast${ffv.unwrapped}"

  given[A: Eq]: Eq[FailFastValidated[A]] = _.unwrapped === _.unwrapped

  given Functor[FailFastValidated]
    def map[A,B] (fa: FailFastValidated[A], f: A => B): FailFastValidated[B] = fa.unwrapped.map(f).wrapped

  given Applicative[FailFastValidated]
    def pure[A](a: A): FailFastValidated[A] = Right(a)

    def ap[A, B] (ff: FailFastValidated[A => B], fa: FailFastValidated[A]): FailFastValidated[B] =
      (ff.unwrapped.toEither, fa.unwrapped.toEither) match
        case (Right(f), Right(a)) => Right(f(a))
        case (Right(_), Left(es)) => Left(es)
        case (Left(es), _       ) => Left(es)

  given Monad[FailFastValidated]
    def flatMap[A,B] (ca: FailFastValidated[A], fc: A => FailFastValidated[B]): FailFastValidated[B] = 
      ca.unwrapped.toEither match 
        case Right(v) => fc(v)
        case Left(v) => Left(v)

  given ApplicativeError[FailFastValidated, Errors]          
    def raise[A](error: Errors): FailFastValidated[A] = Left(error).asValidated.wrapped

    def recover[A](ca: FailFastValidated[A], f: Errors => A): FailFastValidated[A] = 
      ca.unwrapped.recover(f).wrapped

    def fold[A, B] (ca: FailFastValidated[A], fe: Errors => B, fa: A => B): B = ca.unwrapped.toEither.fold(fe, fa)

    override def toEither[A](ca: FailFastValidated[A]): Either[Errors, A] = ca.unwrapped.toEither

    override def fromEither[A](either: Either[Errors, A]): FailFastValidated[A] = either.asValidated.wrapped

  given MonadError[FailFastValidated, Errors] = MonadError.derived[FailFastValidated, Errors]