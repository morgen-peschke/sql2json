package sql2json
package types

import scala.reflect.ClassTag
import cat.Show
import cat.Show.given
import cat.Eq
import cat.Eq.given
import cat.Semigroup
import cat.Semigroup.given
import cat.SemigroupK.given

type Errors = NonEmptyList[String]

/**
 * An attempt to see if `Validated` from [Cats](https://typelevel.org/cats/) could be implemented as a
 * zero-cost wrapper over [[Either]]
 */
opaque type Validated[A] = Either[Errors, A]
object Validated
  trait ValidatedLifts[A]
    def (a: A) valid: Validated[A] = Right(a)
    def[A] (reason: String) invalid: Validated[A] = Left(NonEmptyList.one(reason))
    def[A] (reasons: Errors) invalid: Validated[A] = Left(reasons)

    def (aOpt: Option[A]) asValidated (ifMissing: String): Validated[A] = aOpt.fold(ifMissing.invalid)(_.valid)
    def (aEither: Either[Errors, A]) asValidated: Validated[A] = aEither

  given[A]: ValidatedLifts[A]

  def catchOnly[T <: Throwable]: CatchOnlyPartiallyApplied[T] = new CatchOnlyPartiallyApplied[T]
  final class CatchOnlyPartiallyApplied[T <: Throwable](val ignored: Boolean = true) extends AnyVal
    def apply[A] (body: => A)(given ct: ClassTag[T]): Validated[A] =
      try body.valid
      catch 
        case ct(ex) => ex.toString.invalid

  object Valid {
    def unapply[A](va: Validated[A]): Option[A] = 
      va match 
        case Right(a) => Some(a)
        case _ => None
  }

  given[A](given Show[A]): Show[Validated[A]]
    def show(va: Validated[A]): String = 
      va match
        case Right(a) => s"Valid(${a.show})"
        case Left(e) => s"Invalid(${e.show})"

  given[A: Eq]: Eq[Validated[A]] = 
      (_, _) match
        case (Right(a), Right(b)) => a === b
        case (Left(ae), Left(be)) => ae === be
        case _ => false

  given[A: Semigroup]: Semigroup[Validated[A]] =
      (_, _) match 
        case (Right(a), Right(b)) => Right(a combine b)
        case (Right(_), b @ Left(_)) => b
        case (a @ Left(_), Right(_)) => a
        case (Left(a), Left(b)) => Left(a combineK b)

  given cat.ApplicativeError[Validated]
    type E = Errors
    
    def pure[A](a: A): Validated[A] = a.valid
    
    def raise[A](error: E): Validated[A] = error.invalid

    def recover[A](ca: Validated[A], f: E => A): Validated[A] = ca.fold(f, identity).valid

    def fold[A, B] (ca: Validated[A], fe: E => B, fa: A => B): B = ca.fold(fe, fa)

    override def toEither[A](ca: Validated[A]): Either[Errors, A] = ca

    def ap [A, B] (ff: Validated[A => B], fa: Validated[A]): Validated[B] =
      (ff, fa) match
        case (Right(f), Right(a)) => Right(f(a))
        case (Right(_), Left(es)) => Left(es)
        case (Left(es), Right(_)) => Left(es)
        case (Left(ef), Left(eb)) => Left(ef combineK eb)
    
    def map[A,B] (fa: Validated[A], f: A => B): Validated[B] = fa.map(f)

/**
 * An attempt to stay zero-cost, and introduce a way to tell at the type level if the 
 * underlying [[Either]] is fail-fast or accumulating.
 */
opaque type FailFastValidated[A] = Validated[A]
object FailFastValidated
  
  trait FailFastOps[A]
    def (ffva: FailFastValidated[A]) accumulate: Validated[A] = ffva
    def (va: Validated[A]) failFast: FailFastValidated[A] = va

  given[A]: FailFastOps[A]

  given cat.MonadError[FailFastValidated]
    type E = Errors
    
    def pure[A](a: A): FailFastValidated[A] = Right(a)
    
    def raise[A](error: E): FailFastValidated[A] = Left(error)

    def recover[A](ca: FailFastValidated[A], f: E => A): FailFastValidated[A] = Right(ca.fold(f, identity))

    def fold[A, B] (ca: FailFastValidated[A], fe: E => B, fa: A => B): B = ca.fold(fe, fa)

    def ap[A, B] (ff: FailFastValidated[A => B], fa: FailFastValidated[A]): FailFastValidated[B] =
      (ff, fa) match
        case (Right(f), Right(a)) => Right(f(a))
        case (Right(_), Left(es)) => Left(es)
        case (Left(es), _       ) => Left(es)
    
    def map[A,B] (fa: FailFastValidated[A], f: A => B): FailFastValidated[B] = fa.map(f)

    def flatMap[A,B] (ca: FailFastValidated[A], fc: A => FailFastValidated[B]): FailFastValidated[B] = 
      ca match 
        case Right(v) => fc(v)
        case Left(v) => Left(v)