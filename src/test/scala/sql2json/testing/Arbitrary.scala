package sql2json
package testing

import cat.{Applicative, Functor, Monad}
import cat.Applicative.{~, given}
import cat.Functor.given
import cat.Monad.given
import org.junit.Assert.assertThat
import net.java.quickcheck.QuickCheck
import net.java.quickcheck.{Generator, Characteristic}
import net.java.quickcheck.generator.support.{IntegerGenerator, LongGenerator}
import net.java.quickcheck.generator.PrimitiveGenerators

/**
 * I don't know if QuickTheories implements it's [[Generator]] using mutable
 * internal state, so it's simpler to wrap it in [[Arbitrary]] so I 
 * can work with a fresh one each time and not have to worry about it.
 */
trait Arbitrary[A]
  def gen: Generator[A]

object Arbitrary
  def apply[A](given A: Arbitrary[A]): Arbitrary[A] = A

  def forAll[A: Arbitrary](testName: String)(test: A => Result): Unit = 
    QuickCheck.forAll(
      summon[Arbitrary[A]].gen,
      new Characteristic[A] {
        def name: String = testName
        def setUp(): Unit = ()
        def tearDown(): Unit = ()
        def specify(a: A): Unit = assertThat(test(a), Passes)
      })

  def oneOf[A](arbA0: Arbitrary[A], arbA1: Arbitrary[A], arbAN: Arbitrary[A]*): Arbitrary[A] = 
    val arbAs = arbA0 +: arbA1 +: arbAN.toVector
    new Arbitrary[A] with
      def gen: Generator[A] = 
        new Generator[A] with
          private val genAs: Vector[Generator[A]] = arbAs.map(_.gen)
          private val indexGen = new IntegerGenerator(0, arbAs.length - 1)
          def next(): A = genAs(indexGen.next()).next()

  def choose[A](a0: A, a1: A, aN: A*): Arbitrary[A] = 
    val aVector = a0 +: a1 +: aN.toVector
    new Arbitrary[A] with
      def gen: Generator[A] = 
        new Generator[A] with
          private val indexGen = new IntegerGenerator(0, aVector.length - 1)
          def next(): A = aVector(indexGen.next())

  given Functor[Arbitrary]
    def map [A,B] (fa: Arbitrary[A], f: A => B): Arbitrary[B] =
      new Arbitrary[B] with
        def gen: Generator[B] = 
          new Generator[B] with
            private val wrapped = fa.gen
            def next(): B = f(wrapped.next())

  given Applicative[Arbitrary]
    def pure[A](a: A): Arbitrary[A] = 
      new Arbitrary[A] with
        def gen: Generator[A] = 
          new Generator[A] with
            def next(): A = a

    def ap[A, B](cf: Arbitrary[A => B], ca: Arbitrary[A]): Arbitrary[B] =
      new Arbitrary[B] with
        def gen: Generator[B] = 
          new Generator[B] with
            private val genF = cf.gen
            private val genA = ca.gen
            def next(): B = genF.next()(genA.next())

  given [A,B](given A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A ~ B] = A |@| B

  given Arbitrary[Boolean] = choose[Boolean](true, false)

  given Arbitrary[String]
    def gen = PrimitiveGenerators.strings(500)

  given Arbitrary[Int]
    def gen = 
      new Generator[Int] with
        private val wrapped: Generator[java.lang.Integer] = new IntegerGenerator()
        def next(): Int = wrapped.next().toInt

  given Arbitrary[Long]
      def gen =
        new Generator[Long] with
          private val wrapped: Generator[java.lang.Long] = new LongGenerator()
          def next(): Long = wrapped.next.toLong

  def makeArbFunction[A,B](given CA: Cogen[A], GB: Gen[B], AB: Arbitrary[B]): Arbitrary[A => B] =
    new Arbitrary[A => B] with 
      def gen = 
        new Generator[A => B] with
          private val offsetGenerator: Generator[java.lang.Long] = new LongGenerator()
          def next(): A => B = 
            val offset = offsetGenerator.next()
            (a: A) => GB.fromSeed(new scala.util.Random(offset + CA.toSeed(a)).nextLong)

  given [A,B](given CA: Cogen[A], GB: Gen[B], AB: Arbitrary[B]): Arbitrary[A => B] = makeArbFunction