package sql2json
package types
package validation

import cat.{Functor, Monad}
import cat.Applicative, Applicative.given
import cat.ApplicativeError, ApplicativeError.given
import cat.MonadError, MonadError.given
import cat.Functor.given
import cat.Show, Show.show
import cat.Eq, Eq.given
import cat.Semigroup, Semigroup.given

/**
 * An attempt to stay zero-cost, and introduce a way to tell at the type level if the 
 * underlying [[Either]] is fail-fast or accumulating.
 */
 object FailFast
  opaque type Validated[A] = Either[Errors, A]
  object Validated
    given[A](given Show[A]): Show[Validated[A]] =
      _ match
          case Right(a) => show"FailFast.Valid($a)"
          case Left(e) => show"FailFast.Invalid($e)"

    given[A](given EE: Eq[Either[Errors, A]]): Eq[Validated[A]] = EE

    given[A: Semigroup]: Semigroup[Validated[A]] =
      (_, _) match 
        case (Right(a), Right(b)) => Right(a combine b)
        case (Right(_), b @ Left(_)) => b
        case (a @ Left(_), _) => a

    given Functor[Validated]
      def map[A,B] (fa: Validated[A], f: A => B): Validated[B] = fa.map(f)

    given Applicative[Validated]
      def pure[A](a: A): Validated[A] = Right(a)

      def ap[A, B] (ff: Validated[A => B], fa: Validated[A]): Validated[B] =
        (ff, fa) match
          case (Right(f), Right(a)) => Right(f(a))
          case (Right(_), Left(es)) => Left(es)
          case (Left(es), _       ) => Left(es)

    given ApplicativeError[Validated, Errors]    
      def raise[A](error: Errors): Validated[A] = Left(error)

      def recover[A](ca: Validated[A], f: Errors => A): Validated[A] = Right(ca.fold(f, identity))

      def fold[A, B] (ca: Validated[A], fe: Errors => B, fa: A => B): B = ca.fold(fe, fa)

      override def toEither[A](ca: Validated[A]): Either[Errors, A] = ca

      override def fromEither[A](either: Either[Errors, A]): Validated[A] = either
    
    given Monad[Validated]
      def flatMap[A,B] (ca: Validated[A], fc: A => Validated[B]): Validated[B] = 
        ca match 
          case Right(v) => fc(v)
          case Left(v) => Left(v)

    given MonadError[Validated, Errors] = MonadError.derived[Validated, Errors]
  
  given lifts[A]: AnyRef
    def (a: A) validFF: Validated[A] = Right(a)
    def[A] (reason: String) invalidFF: Validated[A] = Left(NonEmptyList.one(reason))
    def[A] (reasons: Errors) invalidFF: Validated[A] = Left(reasons)

    def (aOpt: Option[A]) asValidatedFF (ifMissing: String): Validated[A] = aOpt.fold(ifMissing.invalidFF)(_.validFF)
    def (aEither: Either[Errors, A]) asValidatedFF: Validated[A] = aEither