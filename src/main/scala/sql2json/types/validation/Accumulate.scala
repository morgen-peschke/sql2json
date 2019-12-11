package sql2json
package types
package validation

import cat.{Functor, Applicative, ApplicativeError}
import cat.Show, Show.show
import cat.Eq, Eq.given
import cat.Semigroup, Semigroup.given
import cat.SemigroupK.given
/**
 * An attempt to see if `Validated` from [Cats](https://typelevel.org/cats/) could be implemented as a
 * zero-cost wrapper over [[Either]]
 *
 * Note: this is _not_ a [[Monad]] as it accumulates,rather than sequences, effects (in this case, errors)
 */
object Accumulate 
  opaque type Validated[A] = Either[Errors, A]
  object Validated
    given[A](given Show[A]): Show[Validated[A]] =
      _ match
          case Right(a) => show"Accumulate.Valid($a)"
          case Left(e) => show"Accumulate.Invalid($e)"

    given[A](given EE: Eq[Either[Errors, A]]): Eq[Validated[A]] = EE

    given[A: Semigroup]: Semigroup[Validated[A]] =
        (_, _) match 
          case (Right(a), Right(b)) => Right(a combine b)
          case (Right(_), b @ Left(_)) => b
          case (a @ Left(_), Right(_)) => a
          case (Left(a), Left(b)) => Left(a combineK b)

    given Functor[Validated]
      def map[A,B] (fa: Validated[A], f: A => B): Validated[B] = fa.map(f)

    given Applicative[Validated]
      def pure[A](a: A): Validated[A] = Right(a)

      def ap [A, B] (ff: Validated[A => B], fa: Validated[A]): Validated[B] =
        (ff, fa) match
          case (Right(f), Right(a)) => Right(f(a))
          case (Right(_), Left(es)) => Left(es)
          case (Left(es), Right(_)) => Left(es)
          case (Left(ef), Left(eb)) => Left(ef combineK eb)

    given ApplicativeError[Validated, Errors]    
      def raise[A](error: Errors): Validated[A] = Left(error)

      def recover[A](ca: Validated[A], f: Errors => A): Validated[A] = Right(ca.fold(f, identity))

      def fold[A, B] (ca: Validated[A], fe: Errors => B, fa: A => B): B = ca.fold(fe, fa)

      override def toEither[A](ca: Validated[A]): Either[Errors, A] = ca

      override def fromEither[A](either: Either[Errors, A]): Validated[A] = either

  
  given lifts[A]: AnyRef 
    def (a: A) valid: Validated[A] = Right(a)
    def[A] (reason: String) invalid: Validated[A] = Left(NonEmptyList.one(reason))
    def[A] (reasons: Errors) invalid: Validated[A] = Left(reasons)

    def (aOpt: Option[A]) asValidated (ifMissing: String): Validated[A] = aOpt.fold(ifMissing.invalid)(_.valid)
    def (aEither: Either[Errors, A]) asValidated: Validated[A] = aEither

  object Valid 
    def unapply[A](va: Validated[A]): Option[A] = 
      va match 
        case Right(a) => Some(a)
        case _ => None
  
  object Invalid 
    def unapply[A](va: Validated[A]): Option[Errors] =
      va match
        case Left(errors) => Some(errors)
        case _ => None

  
