package sql2json
package types

import java.io.StringReader
import Generator.{Action, Result}
import Generator.Action.{halt, given}
import Generator.Result.given
import cat.{Eq,Show,Functor}
import cat.Applicative.{~, given}
import cat.ApplicativeError.given
import cat.SemigroupK.given
import cat.Functor.given
import cat.Monad.given
import cat.MonadError.given
import cat.Eq.given
import cat.Show.show
import org.junit.Test
import org.junit.Assert._
import testing.laws.{ApplicativeLaws, EqLaws, FunctorLaws, MonadLaws, SemigroupLaws}
import testing.{Arbitrary, Gen, Cogen}
import testing.Arbitrary.forAll
import testing.Result.{both, given}
import Done.given

final class ActionEqLaws extends EqLaws[Action[Int]]
final class ActionFunctorLaws extends FunctorLaws[Action, Int, String, Long]

given[A](given AA: Arbitrary[A]): Arbitrary[Action[A]] = 
  Arbitrary.oneOf(
    AA.map(_.continue),
    AA.map(_.stop),
    halt.pure[Arbitrary]
  )

given[A](given GA: Gen[A]): Gen[Action[A]] =
  Gen.usingRandom { rng => 
    rng.nextInt(10) match
      case 0 => GA.fromSeed(rng.nextLong).stop
      case 1 => halt
      case _ => GA.fromSeed(rng.nextLong).continue
  }

given[A](given CA: Cogen[A], CS: Cogen[String], CE: Cogen[NonEmptyList[String]]): Cogen[Action[A]] =
  _ match
    case Action.Continue(a) => 1L + CA.toSeed(a)
    case Action.Stop(a) => 2L + CA.toSeed(a)
    case Action.Halt() => 3L
    case Action.Fail(Result.Failure(gen, errors)) => 4L + CS.toSeed(gen.toString) + CE.toSeed(errors)

final class GeneratorToListTest
  @Test def toListShouldProduceTheExpectedList(): Unit = 
    forAll[List[Long]]("toList (if broken, fix this one first)") { list => 
      Generator.fromList(list).toList <-> list.success
    }

final class GeneratorEmptyTest
  @Test def foldLeftShouldReturnInitial(): Unit = 
    forAll[Int ~ Action[Int]]("no-op foldLeft") { 
      case initial ~ ignoredResult => 
        Generator.empty[Int].foldLeft[Int](initial, (a,b) => ignoredResult) <-> initial.success
    }

  @Test def foreachShouldSkipBody(): Unit = 
    Generator.empty[Int].foreach { a => 
      assertFail(s"Body received $a instead of being skipped")
      Done.stop
    } <-> Done.success

  @Test def foldShouldReturnEmpty(): Unit = 
    Generator.empty[Long].fold <-> 0L.success

  @Test def foldKShouldReturnEmpty(): Unit = 
    Generator.empty[Long].foldK[List] <-> List.empty[Long].success

final class GeneratorOneTest
  @Test def foldLeftShouldOnlyRunOnce(): Unit = 
    forAll[Int ~ Int ~ (Int => Action[Int])]("foldLeft") {
      case base ~ initial ~ f =>
        Generator.one(base).foldLeft(initial, (a,b) => f(a + b)) <-> f(base + initial).asResult(initial)
    }

  @Test def foreachShouldOnlyRunOnce(): Unit = 
    forAll[Int]("foreach") { base => 
      var counter = 0
      both(
        "generator results" asClue {
          Generator.one(base).foreach { value => 
            counter += value
            Done.continue
          } <-> Done.success
        },
        "counter value" asClue(counter <-> base)
      )
    }

  @Test def foldShouldReturnTheValue(): Unit = 
    forAll[Long]("fold") { base => 
      Generator.one(base).fold <-> base.success
    }

  @Test def foldKShouldReturnTheWrappedValue(): Unit = 
    forAll[Long]("foldK") { base => 
      Generator.one(base).foldK <-> (base :: Nil).success
    }

final class SimpleGeneratorVariantTests
  @Test def fromListShouldRunOncePerElement(): Unit = 
    forAll[List[Int]]("Generator.fromList(...).foldLeft") { list => 
      Generator.fromList(list).foldLeft(
        List.empty[Int], 
        (a,b) => (b :: a).continue
      ) <-> list.reverse.success 
    }

  @Test def constShouldProduceTheSameValueEndlessly(): Unit = 
    given Arbitrary[Int] = Arbitrary.between(0, 10)
    forAll[Int ~ Long]("Generator.const(...).foldLeft") { 
      case count ~ element => 
        Generator
          .const(element)
          .foldLeft(
            count, 
            (remaining, b) => {
              assertEquals(element, b)
              if remaining == 0 then 0.stop
              else (remaining - 1).continue
            }
          ) <-> 0.success
    }

  @Test def continuallyShouldRunTheSameEffectEndlessly(): Unit = 
    given Arbitrary[Int] = Arbitrary.between(0, 10)
    forAll[Int ~ Long]("Generator.const(...).foldLeft") { 
      case count ~ element => 
        var remainingGlobal = count
        both(
          "[elements match] " asClue {
            Generator
              .continually {
                val element = remainingGlobal
                remainingGlobal = remainingGlobal - 1
                element
              }
              .foldLeft(
                count, 
                (remaining, element) => {
                  assertEquals(element, remaining)
                  if remaining == 0 then 0.stop
                  else (remaining - 1).continue
                }
              ) <-> 0.success
          },
          "[remainingGlobal matches] " asClue {
            remainingGlobal <-> -1
          }
        )
    }

  @Test def calculateShouldUseThePreviousValueToProduceTheNextValue(): Unit = 
    given Arbitrary[Int] = Arbitrary.between(0, 10)
    forAll[Long ~ Int]("Generator.calculate(...).foldLeft") { 
      case base ~ length => 
        val last = base + length.toLong
        Generator
          .calculate(
            base,
            { _ match
              case `last` => last.stop
              case i => (i + 1L).continue
            }
          )
          .foldLeft[List[Long]](
            Nil,
            (a, b) => (b :: a).continue
          )
          .map(_.reverse) <-> (base to last).toList.success
    }

  @Test def fromShouldCountUpwardsFromStartByStep(): Unit =
    given Arbitrary[Long] = Arbitrary.between(-2000, 2000).map(_.toLong)
    given Arbitrary[Int] = Arbitrary.between(0, 10)
    forAll[Long ~ Int]("Generator.from(start, step).toList equivalence with Iterator.from") {
      case start ~ step =>
        Generator.from[Long](start, step.toLong).take(20).toList <-> Iterator.from(start.toInt, step).take(20).map(_.toLong).toList.success
    }

  @Test def unfoldShouldExpandUntilExhausted(): Unit = 
    forAll[String]("Generator.unfold(...).foldLeft") { string =>
      given Show[StringReader] = _.toString
      val generator = 
        for 
          reader <- Generator.ofResource(
                      s"StringReader($string)",
                      () => new StringReader(string),
                      _.close().done
                    )
          char  <- Generator.unfold(
                      reader,
                      { _.read() match
                        case -1 => halt
                        case i => i.toChar.continue
                      }
                    )
        yield char
      generator.foldLeft(
        new StringBuilder,
        (b, c) => b.append(c).continue
      ).map(_.toString) <-> string.success
    }

  @Test def takeShouldBehaveLikeTakeOnList(): Unit = 
    given Arbitrary[(List[Int], Int)] =  
      for
        list <- summon[Arbitrary[List[Int]]]
        take <- Arbitrary.between(0, list.length + 10)
      yield (list, take)

    forAll[(List[Int], Int)]("Generator#take(...).toList equivalence with List.take") { 
      case (list, length) =>
        Generator.fromList(list).take(length.toLong).toList <-> list.take(length).success
    }

  @Test def takeWhileShouldBehaveLikeTakeWhileOnList(): Unit = 
    given: Arbitrary[Int] = Arbitrary.between(0, 20)
    forAll[Int]("Generator#takeWhile(...).toList equivalence with List.takeWhile") { length =>
      Generator.from[Int](0).takeWhile(_ < length).toList <-> (0 to length + 3).takeWhile(_ < length).toList.success
    }

  @Test def takeUntilShouldOmitTheSentinel(): Unit = 
    given: Arbitrary[Int] = Arbitrary.between(0, 20)
    forAll[Int]("Generator#takeUntil(...).toList equivalence with List.takeWhile") { length =>
      Generator.from[Int](0).takeUntil(length).toList <-> (0 to length + 3).takeWhile(_ != length).toList.success
    }

  @Test def dropUntilShouldOmitLeadingSentinelValues(): Unit = 
    given: Arbitrary[Int] = Arbitrary.between(0, 20)
    forAll[Int ~ NonEmptyList[Long]]("Generator#dropUntil(...).toList equivalence with List.dropWhile") { 
      case padding ~ nel => 
        // The inversion of nel.head is to ensure we don't run into a situation where the next element past the sentinels
        // we add is also the sentinel value
        val base = Generator.const(nel.head).take(padding.toLong) combineK Generator.fromList(-nel.head :: nel.tail)
        base.dropWhile(nel.head).toList <-> (-nel.head :: nel.tail).success
    }

object GeneratorLawTests
  given[A,B]: Show[A => B] = _.toString
  given[A: Show](given ALA: Arbitrary[List[A]]): Arbitrary[Generator[A]] = ALA.map(Generator.fromList)
  given[A](given Eq[Result[List[A]]]): Eq[Generator[A]] = _.toList === _.toList
  given[A](given Show[Result[List[A]]]): Show[Generator[A]] = _.toList.show
  given[A](given CA: Cogen[Result[List[A]]]): Cogen[Generator[A]] = g => CA.toSeed(g.toList)
  given[A: Show](given GA: Gen[List[A]]): Gen[Generator[A]] = GA.map(Generator.fromList)

  final class GeneratorFunctorLaws extends FunctorLaws[Generator, Int, String, Long]
  final class GeneratorApplicativeLaws extends ApplicativeLaws[Generator, Int, String, Long]
  final class GeneratorMonadLaws extends MonadLaws[Generator, Int, String]
  final class GeneratorSemigroupLaws extends SemigroupLaws[Generator[Int]]