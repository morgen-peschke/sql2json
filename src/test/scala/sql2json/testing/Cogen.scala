package sql2json
package testing

/**
 * Dual of [[Gen]], mostly used to create instances
 * of `Arbitrary[A => B]`
 */
trait Cogen[A]
  def toSeed(a: A): Long

object Cogen
  def apply[A](given C: Cogen[A]): Cogen[A] = C

  given Cogen[Int] = _.toLong
  given Cogen[Long] = identity(_)
  given Cogen[String] = _.foldLeft(0L)(_ + _.hashCode.toLong)