package sql2json
package cat

import Applicative.given
import types.Convertible
import Applicative.given
import types.Convertible
import Convertible.given
import scala.quoted.{Type, Expr, QuoteContext}
import scala.reflect.ClassTag

trait ApplicativeError[C[_], E](given val applicative: Applicative[C])
  def raise[A](error: E): C[A]

  def recover[A](ca: C[A], f: E => A): C[A]

  def fold[A, B] (ca: C[A], fe: E => B, fa: A => B): B

  def mapError[A, EO <: E](cae: C[A], fe: E => EO): C[A] =
    fold(cae, e => raise(fe(e)), _.pure)

  def toEither[A](ca: C[A]): Either[E, A] = fold(ca, Left(_), Right(_))
  
  def fromEither[A](either: Either[E,A]): C[A] = 
    either match
      case Left(e) => raise[A](e)
      case Right(a) => a.pure[C]

  def catchOnly[T <: Throwable]: ApplicativeError.CatchOnlyPartiallyApplied[C,E,T] = 
    new ApplicativeError.CatchOnlyPartiallyApplied[C,E,T](given this)

trait MonadErrorProvidesApplicativeError
  given [F[_], E] (given ME: MonadError[F, E]): ApplicativeError[F, E] = ME.applicativeError

object ApplicativeError extends MonadErrorProvidesApplicativeError
  final class CatchOnlyPartiallyApplied[C[_], E, T <: Throwable](given AE: ApplicativeError[C,E]) extends AnyVal
    def apply[A] (body: => A)(given CTE: Convertible[T,E], CT: ClassTag[T]): C[A] = 
        try AE.applicative.pure(body)
        catch 
        case CT(ex) => AE.raise(CTE.cast(ex))

  given lifts[E]: AnyRef
    def[C[_], A] (error: E) raise (given AE: ApplicativeError[C,E]): C[A] = AE.raise[A](error)
  
  given eitherLifts[E,A]: AnyRef
    def[C[_]] (either: Either[E,A]) liftToError (given AE: ApplicativeError[C, E]): C[A] = AE.fromEither(either)

  given ops[C[_],A]: AnyRef
    def[E] (ca: C[A]) recover (f: E => A)(given AE: ApplicativeError[C,E]): C[A] = AE.recover(ca, f)

    def[B, E] (ca: C[A]) fold(fe: E => B, fa: A => B)(given AE: ApplicativeError[C,E]): B = AE.fold(ca, fe, fa)

    def[E] (cae: C[A]) mapError (fe: E => E)(given AE: ApplicativeError[C,E]): C[A] = AE.mapError[A, E](cae, fe)

    def[E] (cae: C[A]) toEither (given AE: ApplicativeError[C,E]): Either[E, A] = AE.toEither(cae)

  given[C0[_], C1[_], E](given AE1: ApplicativeError[C0, E], AE2: ApplicativeError[C1, E]): types.ConvertibleK[C0, C1]
    def castK[A](a: C0[A]): C1[A] = a.toEither.liftToError[C1]
