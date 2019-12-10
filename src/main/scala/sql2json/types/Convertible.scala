package sql2json
package types

trait Convertible[-A,+B]
  def cast(a: A): B

object Convertible
  trait ConvertibleOps[A]
    def[B] (a: A) as (given CAB: Convertible[A,B]): B = CAB.cast(a)

  given[A]: ConvertibleOps[A]