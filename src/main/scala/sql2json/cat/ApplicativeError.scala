package sql2json
package cat

import Applicative.given
import Bifunctor.given

trait ApplicativeError[C[_]](given val applicative: Applicative[C])
  type E

  def raise[A](error: E): C[A]

  def recover[A](ca: C[A], f: E => A): C[A]

  def fold[A, B] (ca: C[A], fe: E => B, fa: A => B): B

  def mapError[A, EO <: E](cae: C[A], fe: E => EO): C[A] =
    fold(cae, e => raise(fe(e)), _.pure)

  def toEither[A](ca: C[A]): Either[E, A] = fold(ca, Left(_), Right(_))
  
object ApplicativeError
  type Aux[C[_], E] = ApplicativeError[C] with
    type E

  trait ApplicativeErrorLifts[E]
    def[C[_], A] (error: E) raise (given AE: Aux[C,E], ev: E <:< AE.E): C[A] = AE.raise[A](ev(error))

  trait ApplicativeErrorOps[C[_],A]
    def[E] (ca: C[A]) recover (f: E => A)(given AE: Aux[C,E], ev: AE.E <:< E): C[A] =
      AE.recover(ca, f compose ev)

    def[B, E] (ca: C[A]) fold(fe: E => B, fa: A => B)(given AE: Aux[C,E], ev: AE.E <:< E): B =
      AE.fold(ca, fe compose ev, fa)

    def[E] (cae: C[A]) mapError (fe: E => E)(given AE: Aux[C,E], ev1: AE.E <:< E, ev2: E <:< AE.E): C[A] =
      AE.mapError[A, AE.E](cae, e => ev2(fe(ev1(e))))

    def[E] (cae: C[A]) toEither (given AE: Aux[C,E], ev: AE.E <:< E): Either[E, A] = 
      AE.toEither(cae).leftMap(ev)

  given[E]: ApplicativeErrorLifts[E]
  given[C[_],A]: ApplicativeErrorOps[C,A]