package sql2json
package testing

import scala.util.Random

/**
 * While I'm using QuickCheck, I still need this to be able to 
 * create `Arbitrary[A => B]` instances.
 */
trait Gen[A]
  def fromSeed(s: Long): A

object Gen
  def apply[A](given G: Gen[A]): Gen[A] = G

  def usingRandom[A](body: Random => A): Gen[A] = seed => body(new Random(seed))

  given Gen[Int] = _.toInt
  given Gen[Long] = identity(_)
  given Gen[String] = usingRandom(rng => rng.nextString(rng.nextInt(100)))