package sql2json
package cat

trait Empty[A]
  def empty: A

object Empty
  def instance[A](zero: A): Empty[A] = 
    new Empty[A] with
      def empty: A = zero

  given Empty[Long] = instance[Long](0L)