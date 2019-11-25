package sql2json
package testing

/**
 * While I'm using QuickCheck, I still need this to be able to 
 * create `Arbitrary[A => B]` instances.
 */
trait Gen[A]
  def fromSeed(s: Long): A

object Gen
  def apply[A](given G: Gen[A]): Gen[A] = G

  given Gen[Int] = _.toInt
  given Gen[Long] = identity(_)
  given Gen[String] = 
    seed => 
      val rng = new scala.util.Random(seed)
      rng.nextString(rng.nextInt(100))