package sql2json
package types

import cat.Functor
import cat.Cofunctor, Cofunctor.given

trait Convertible[A,B]
  def cast(a: A): B

  def map[B1](f: B => B1): Convertible[A,B1] = a => f(cast(a))

  def comap[A0](f: A0 => A): Convertible[A0, B] = a0 => cast(f(a0))

trait ConvertibleKProvidesConvertible
  given[A[_],B[_],C](given CAB: ConvertibleK[A,B]): Convertible[A[C],B[C]] = 
    CAB.convertible[C]

object Convertible extends ConvertibleKProvidesConvertible
  def instance[A,B](f: A => B): Convertible[A,B] = f(_)

  given ops[A]: AnyRef
    def[B](a: A) as (given CAB: Convertible[A,B]): B = CAB.cast(a)

  given [A]: Convertible[A,A] = identity(_)

trait ConvertibleK[A[_], B[_]]
  def castK[C](a: A[C]): B[C]

  def convertible[C]: Convertible[A[C], B[C]] = castK[C](_)

object ConvertibleK
  given ops[A[_], C]: AnyRef
    def[B[_]](a: A[C]) asKind (given CAB: ConvertibleK[A,B]): B[C] = CAB.castK(a)

  given[A[_]]: ConvertibleK[A,A]
    def castK[C](a: A[C]): A[C] = a