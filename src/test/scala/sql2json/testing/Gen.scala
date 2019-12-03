package sql2json
package testing

import cat.Functor
import types.NonEmptyList
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

  given Functor[Gen]
    def map [A,B] (fa: Gen[A], f: A => B): Gen[B] = seed => f(fa.fromSeed(seed))

  given Gen[Int] = _.toInt
  given Gen[Long] = identity(_)
  given Gen[String] = usingRandom(rng => rng.nextString(rng.nextInt(100)))
  given Gen[Boolean] = usingRandom(_.nextBoolean)

  given[A](given GA: Gen[A]): Gen[List[A]] = usingRandom { rng => 
    List.fill(rng.nextInt(20))(rng.nextLong).map(GA.fromSeed)
  }

  given[A](given GA: Gen[A]): Gen[NonEmptyList[A]] = usingRandom { rng => 
    val size = rng.nextInt(20)
    val head = GA.fromSeed(rng.nextLong)
    val tail = List.fill(size - 1)(rng.nextLong).map(GA.fromSeed)
    NonEmptyList(head, tail)
  }