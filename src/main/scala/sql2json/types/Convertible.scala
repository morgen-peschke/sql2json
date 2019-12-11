package sql2json
package types

trait Convertible[-A,+B]
  def cast(a: A): B

trait ConvertibleKProvidesConvertible
  given[A[_],B[_],C](given CAB: ConvertibleK[A,B]): Convertible[A[C],B[C]] = 
    CAB.convertible[C]

object Convertible extends ConvertibleKProvidesConvertible
  given ops[A]: AnyRef
    def[B](a: A) as (given CAB: Convertible[A,B]): B = CAB.cast(a)

  given [A]: Convertible[A,A] = identity(_)

trait ConvertibleK[-A[_], +B[_]]
  def castK[C](a: A[C]): B[C]

  def convertible[C]: Convertible[A[C], B[C]] = castK[C](_)

object ConvertibleK
  given ops[A[_], C]: AnyRef
    def[B[_]](a: A[C]) asKind (given CAB: ConvertibleK[A,B]): B[C] = CAB.castK(a)

  given[A[_]]: ConvertibleK[A,A]
    def castK[C](a: A[C]): A[C] = a