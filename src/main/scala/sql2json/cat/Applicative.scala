package sql2json
package cat

import Applicative.~
import Functor.given

trait Applicative[C[_]](given val functor: Functor[C])
  def pure[A](a: A): C[A]

  def ap[A, B](cf: C[A => B], ca: C[A]): C[B]

  def zip[A,B](ca: C[A], cb: C[B]): C[A ~ B] =
    ap(ca.map(a => (b: B) => (a,b)),cb)

  def productR[A,B](ca: C[A], cb: C[B]): C[B] = 
    ap(ca.map(_ => identity(_: B)), cb)
      
  def productL[A,B](ca: C[A], cb: C[B]): C[A] = 
    ap(ca.map(a => (_: B) => a), cb)

object Applicative
  type ~[A,B] = (A,B)
  object ~
    def unapply[A,B](ab: (A,B)): (A,B) = ab

  trait ApplicativeLifts[A]
    def[C[_]](a: A) pure (given C: Applicative[C]): C[A] = C.pure(a)

  trait ApplicativeOps[C[_], A]
    def[B](cf: C[A => B]) ap (ca: C[A])(given AP: Applicative[C]): C[B] = AP.ap(cf, ca)

    def[B](ca: C[A]) |@| (cb: C[B])(given AP: Applicative[C]): C[A ~ B] = AP.zip(ca, cb)

    def[B](ca: C[A]) *> (cb: C[B])(given AP: Applicative[C]): C[B] = AP.productR(ca, cb)

    def[B](ca: C[A]) <* (cb: C[B])(given AP: Applicative[C]): C[A] = AP.productL(ca, cb)

  given[A]: ApplicativeLifts[A]
  given[C[_],A]: ApplicativeOps[C, A]