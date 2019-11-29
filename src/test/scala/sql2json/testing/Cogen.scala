package sql2json
package testing

import types.NonEmptyList
import cat.Functor.given

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
  given Cogen[Boolean] = if _ then 0L else 1L
  given [A](given A: Cogen[A]): Cogen[NonEmptyList[A]] = _.map(A.toSeed).fold