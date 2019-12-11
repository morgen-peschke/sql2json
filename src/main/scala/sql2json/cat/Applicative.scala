package sql2json
package cat

import Applicative.~
import Functor.given
import scala.compiletime.summonFrom

trait Applicative[C[_]](given val functor: Functor[C])
  def pure[A](a: A): C[A]

  def ap[A, B](cf: C[A => B], ca: C[A]): C[B]

  def product[A,B](ca: C[A], cb: C[B]): C[A ~ B] =
    ap(ca.map(a => (b: B) => (a,b)),cb)

  def productR[A,B](ca: C[A], cb: C[B]): C[B] = 
    ap(ca.map(_ => identity(_: B)), cb)
      
  def productL[A,B](ca: C[A], cb: C[B]): C[A] = 
    ap(ca.map(a => (_: B) => a), cb)

trait MonadAlsoProvidesApplicative
  given[F[_]] (given M: Monad[F]): Applicative[F] = M.applicative

trait ApplicativeErrorProvidesApplicative extends MonadAlsoProvidesApplicative
  given[F[_]] (given AE: ApplicativeError[F, ?]): Applicative[F] = AE.applicative

object Applicative extends ApplicativeErrorProvidesApplicative
  type ~[A,B] = (A,B)
  object ~
    def unapply[A,B](ab: (A,B)): (A,B) = ab

  given lifts[A]: AnyRef 
    def[C[_]](a: A) pure (given C: Applicative[C]): C[A] = C.pure(a)
  
  given ops[C[_], A]: AnyRef
    def[B](cf: C[A => B]) ap (ca: C[A])(given AP: Applicative[C]): C[B] = AP.ap(cf, ca)
   
    def[B](cf: C[A => B]) <*> (ca: C[A])(given AP: Applicative[C]): C[B] = AP.ap(cf, ca)
    
    def[B](ca: C[A]) |@| (cb: C[B])(given AP: Applicative[C]): C[A ~ B] = AP.product(ca, cb)

    def[B](ca: C[A]) *> (cb: C[B])(given AP: Applicative[C]): C[B] = AP.productR(ca, cb)

    def[B](ca: C[A]) <* (cb: C[B])(given AP: Applicative[C]): C[A] = AP.productL(ca, cb)

  given Applicative[List]
    def pure[A](a: A): List[A] = a :: Nil
    def ap[A, B](cf: List[A => B], ca: List[A]): List[B] = cf.flatMap(ca.map(_))