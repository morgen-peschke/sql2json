package sql2json
package types

import cat.{Applicative, Eq, Functor, Semigroup, SemigroupK, Monad, Monoid, MonoidK, Show}
import cat.Applicative.given
import cat.ApplicativeError.given
import cat.Functor.given
import cat.Semigroup.given
import cat.SemigroupK.given
import cat.Show.given
import cat.Eq.given
import Done.given
import Generator.{Action, given}
import Generator.Action.{halt, given}
import Generator.Result.given

import scala.reflect.ClassTag
import scala.annotation.tailrec

/**
 * The idea for this was shamelessly ripped off of [Li Haoyi's `geny`](https://github.com/lihaoyi/geny).
 * 
 * The implementation is my own, so don't judge his code by this.
 */
trait Generator[A]
  def name: String

  def foldLeft[B](initial: B, f: (B, A) => Action[B]): Generator.Result[B]

  def foreach(f: A => Action[Done]): Generator.Result[Done] = 
    foldLeft(Done.upcast, (_, v) => f(v).map(_ => Done))
  
  def fold(given M: Monoid[A]): Generator.Result[A] = 
    foldLeft(
      M.empty, 
      (accum, v) => Action.Continue(accum combine v)
    )

  def foldK[C[_]: Applicative](given MK: MonoidK[C]): Generator.Result[C[A]] = 
    foldLeft[C[A]](
      MK.emptyK, 
      (accum, v) => Action.Continue(accum combineK v.pure)
    )

  def map[B](f: A => B): Generator[B] = new Generator.Map(f, this)
  def flatMap[B](f: A => Generator[B]): Generator[B] = new Generator.FlatMap(f, this)
  def take(count: Long): Generator[A] = new Generator.Take(count, this)
  def takeWhile(p: A => Boolean): Generator[A] = new Generator.TakeWhile(p, this)
  def takeUntil(sentinel: A)(given Show[A], Eq[A]): Generator[A] = new Generator.TakeUntil(sentinel, this)
  def dropWhile(sentinel: A)(given Show[A], Eq[A]): Generator[A] = new Generator.DropWhile(sentinel, this)

  /**
   * Materialize to a list - do not call on infinite generators
   */
  def toList: Generator.Result[List[A]] = 
    foldLeft(
      new scala.collection.mutable.ListBuffer[A],
      (buffer, element) => buffer.append(element).continue 
    ).map(_.toList)

object Generator
  given Show[Generator[?]] = _.name

  sealed abstract class Result[A]
    def asValidated: Validated[A] = this match
      case Result.Success(a) => a.pure[Validated]
      case Result.Failure(generator, errors) => errors.map(e => s"$generator: $e").raise[Validated, A]

    def asAction: Action[A] = this match
      case Result.Success(a) => Action.Continue(a)
      case f @ Result.Failure(_, _) => Action.Fail(f)

  object Result
    case class Success[A](value: A) extends Result[A]
    case class Failure[A](generator: Generator[?], errors: Errors) extends Result[A]
      def as[B]: Failure[B] = this.asInstanceOf[Failure[B]]

    trait ResultOps[A]
      def (a: A) success: Result[A] = Success(a)
      def (a: Generator[A]) failure (reason: String): Result[A] = Failure(a, reason.pure[NonEmptyList])

    given[A]: ResultOps[A]
      
    given[A: Eq]: Eq[Result[A]] =
      (_, _) match
        case (Success(a), Success(b)) => a === b
        case (Failure(gA, errorsA), Failure(gB, errorsB)) => gA == gB && errorsA == errorsB
        case _ => false

    given[A: Show]: Show[Result[A]] = _ match
      case Success(a) => show"Success($a)"
      case Failure(gen, errors) => show"Failure($gen, $errors)"

    given Functor[Result]
      def map [A,B] (fa: Result[A], f: A => B): Result[B] = 
        fa match 
          case Success(a) => Success(f(a))
          case failure @ Failure(_, _) => failure.as[B]

  sealed abstract class Action[A]
    def asResult(fallback: A): Result[A] = this match
      case Action.Continue(a) => Result.Success(a)
      case Action.Stop(a) => Result.Success(a)
      case Action.Halt() => Result.Success(fallback)
      case Action.Fail(failure) => failure

  object Action
    // Keep doing what you're doing
    case class Continue[A](result: A) extends Action[A]

    // Stop the generator (one last result)
    case class Stop[A](result: A) extends Action[A]

    // Stop the generator (no final result)
    case class Halt[A]() extends Action[A]

    // Fail the whole chain. Should be propagated out
    case class Fail[A](result: Result.Failure[A]) extends Action[A]

    given Functor[Action]
      def map [A,B] (fa: Action[A], f: A => B): Action[B] = 
        fa match 
          case Continue(a) => Continue(f(a))
          case Stop(a) => Stop(f(a))
          case Halt() => Halt()
          case Fail(failure) => Fail(failure.as[B])

    trait ActionOps[A]
      def (a: A) continue: Continue[A] = Continue(a)
      def (a: A) stop: Stop[A] = Stop(a)

    given[A]: ActionOps[A]
    
    def halt[A]: Action[A] = Halt()

    given[A: Show]: Show[Action[A]] =
       _ match
        case Continue(r) => show"Continue($r)"
        case Stop(r) => show"Stop($r)"
        case Halt() => "Halt()"
        case Fail(failure) => show"Fail($failure)"

    given[A: Eq]: Eq[Action[A]] =
      (_, _) match
        case (Continue(a), Continue(b)) => a === b
        case (Stop(a), Stop(b)) => a === b
        case (Halt(), Halt()) => true 
        case (Fail(a), Fail(b)) => a == b
        case _ => false

  def empty[A]: Generator[A] = new Empty[A]
  def one[A: Show](value: A): Generator[A] = new One(value)
  def fromList[A: Show](values: List[A]): Generator[A] = new FromList(values)
  def fromStream[A](stream: () => java.util.stream.Stream[A]): Generator[A] = new FromStream(stream)
  def const[A: Show](value: A): Generator[A] = new Const(value)
  def continually[A](value: => A): Generator[A] = new Continually(value)
  def calculate[A: Show](seed: A, step: A => Action[A]): Generator[A] = new Calculate(seed, step)
  def from[A: Show](given N: scala.math.Numeric[A])(start: A, step: A = N.one): Generator[A] = new From(start, step)
  def ofResource[A](name: String, aquire: () => A, cleanup: A => Done): Generator[A] = new OfResource(name, aquire, cleanup)
  def unfold[A: Show,B](seed: A, expand: A => Action[B]): Generator[B] = new Unfold(seed, expand)

  given Functor[Generator]
    def map [A,B] (fa: Generator[A], f: A => B): Generator[B] = fa.map(f)

  given Applicative[Generator]
    def pure[A](a: A): Generator[A] = Generator.one(a)(given _.toString)
    def ap[A, B](cf: Generator[A => B], ca: Generator[A]): Generator[B] = cf.flatMap(ca.map(_))

  given Monad[Generator]
    def flatMap[A,B](ca: Generator[A], fc: A => Generator[B]): Generator[B] = ca.flatMap(fc)

  given SemigroupK[Generator]
    def combineK[A] (a: Generator[A], b: Generator[A]): Generator[A] = new Concat(a, b)

  class Empty[A] extends Generator[A]
    def name: String = "Generator.empty"
    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] = initial.success
    override def foreach(f: A => Action[Done]): Result[Done] = Done.success
    override def fold(given M: Monoid[A]): Result[A] = M.empty.success
    override def foldK[C[_]: Applicative](given MK: MonoidK[C]): Result[C[A]] = MK.emptyK[A].success

  class One[A: Show](value: A) extends Generator[A]
    def name: String = show"Generator.one($value)"
    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] = f(initial, value).asResult(initial)
    override def foreach(f: A => Action[Done]): Result[Done] = f(value).asResult(Done)
    override def fold(given Monoid[A]): Result[A] = value.success
    override def foldK[C[_]: Applicative: MonoidK]: Result[C[A]] = value.pure.success

  class FromList[A: Show](values: List[A]) extends Generator[A]
    def name: String = show"Generator.ofList($values)"
    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] = 
      @tailrec
      def loop(accum: B, remaining: List[A]): Result[B] = 
        remaining match
          case Nil => accum.success
          case value :: rest =>
            f(accum, value) match
              case Action.Continue(result) => loop(result, rest)
              case action => action.asResult(accum)
          
      loop(initial, values)

  class FromStream[A](stream: () => java.util.stream.Stream[A]) extends Generator[A]
    def name: String = "Generator.fromStream(???)"

    private class CollectorState[B](var current: Action[B])

    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] = 
      stream()
        .collect(java.util.stream.Collector.of(
          () => CollectorState(initial.continue),
          (state, element) => state.current = state.current match {
            case Action.Continue(accum) => f(accum, element)
            case action => action
          },
          (_, _) => throw new IllegalStateException("Merge not supported"),
          _.current.asResult(initial)
        ))
    

  class Const[A: Show](value: A) extends Generator[A]
    def name: String = show"Generator.const($value)"
    
    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] =
      @tailrec
      def loop(accum: B): Result[B] = 
        f(accum, value) match
          case Action.Continue(result) => loop(result)
          case action => action.asResult(accum)
      
      loop(initial)
    
  class Continually[A](value: => A) extends Generator[A]
    def name: String = "Generator.continually(???)"
    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] =
      @tailrec
      def loop(accum: B): Result[B] = 
        f(accum, value) match
          case Action.Continue(result) => loop(result)
          case action => action.asResult(accum)

      loop(initial)

  class Calculate[A: Show](seed: A, step: A => Action[A]) extends Generator[A]
    def name: String = show"Generator.calculate(seed = $seed)"

    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] =
      @tailrec
      def loop(accum: B, prev: A): Result[B] = 
        step(prev) match
          case Action.Continue(next) => f(accum, next) match
            case Action.Continue(result) => loop(result, next)
            case action => action.asResult(accum)
          case action => action.map(_ => accum).asResult(accum)
          
      f(initial, seed) match
        case Action.Continue(result) => loop(result, seed)
        case action => action.asResult(initial)
        
  class From[A: Show](start: A, step: A)(given N: scala.math.Numeric[A]) extends Generator[A]
    def name: String = s"Generator.from(start=$start, step=$step)"

    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] =
      @tailrec
      def loop(accum: B, prev: A): Result[B] = 
        val next = N.plus(prev, step)
        f(accum, next) match
          case Action.Continue(result) => 
            loop(result, next)
          case action => 
            action.asResult(accum)
          
      f(initial, start) match
        case Action.Continue(result) => loop(result, start)
        case action => action.asResult(initial)


  class OfResource[A](named: String, aquire: () => A, cleanup: A => Done) extends Generator[A]
    def name: String = s"Generator.ofResource($named)"
    
    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] =
      val a = aquire()
      try f(initial, a).asResult(initial)
      finally cleanup(a)

  class Unfold[A: Show,B](seed: A, expand: A => Action[B]) extends Generator[B]
    def name: String = show"Generator.unfold(seed = $seed)"

    def foldLeft[C](initial: C, f: (C, B) => Action[C]): Result[C] =
      @tailrec
      def loop(accum: C): Result[C] = 
        expand(seed) match
          case Action.Continue(next) => f(accum, next) match
            case Action.Continue(result) => loop(accum)
            case action => action.asResult(accum)
          case Action.Stop(next) => f(accum, next).asResult(accum)
          case action => action.map(_ => accum).asResult(accum)
            
      loop(initial)

  class Map[A,B](fab: A => B, base: Generator[A]) extends Generator[B]
    def name: String = s"${base.show}.map($fab)"
    
    def foldLeft[C](initial: C, fcb: (C, B) => Action[C]): Result[C] = 
      base.foldLeft(initial, (accum, value) => fcb(accum, fab(value)))
     
  class FlatMap[A,B](fab: A => Generator[B], base: Generator[A]) extends Generator[B]
    def name: String = s"${base.show}.flatMap($fab)"

    def foldLeft[C](initial: C, fcb: (C, B) => Action[C]): Result[C] = 
      base.foldLeft(
        initial, 
        (outerAccum, outerValue) => 
          fab(outerValue).foldLeft(
            outerAccum,
            (innerAccum, innerValue) =>
              fcb(innerAccum, innerValue)
          ).asAction
      )
  
  class Take[A](amount: Long, base: Generator[A]) extends Generator[A]
    def name: String = show"$base.take($amount)"

    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] =
      base.foldLeft(
        (initial, amount), 
        (accumAndCount, value) => {
          accumAndCount match 
            case result @ (_, 0L) => result.stop
            case (accum, remaining) => 
              f(accum, value) match 
                case Action.Continue(result) => (result, remaining - 1L).continue
                case Action.Stop(result) => (result, 0L).stop
                case Action.Halt() => (accum, 0L).stop
                case Action.Fail(failure) => Action.Fail(failure.as[(B, Long)])
        }).map(_._1)

  class TakeWhile[A](predicate: A => Boolean, base: Generator[A]) extends Generator[A]
    def name: String = s"${base.show}.while($predicate)"

    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] =
      base.foldLeft(
        initial, 
        (accum, value) => {
          if (predicate(value)) then f(accum, value)
          else accum.stop
        })
    
  class TakeUntil[A: Show: Eq](sentinel: A, base: Generator[A]) extends Generator[A]
    def name: String = show"$base.until($sentinel)"

    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] =
      base.foldLeft(
        initial, 
        (accum, value) => {
          if value === sentinel then halt
          else f(accum, value)
        })

  class Concat[A](first: Generator[A], second: Generator[A]) extends Generator[A]
    def name: String = show"$first.concat($second)"

    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] =
      first.foldLeft(initial, f) match
        case f @ Result.Failure(_, _) => f
        case Result.Success(result) => second.foldLeft(result, f)

  class DropWhile[A: Show: Eq](sentinel: A, base: Generator[A]) extends Generator[A]
    def name: String = show"$base.dropWhile($sentinel)"

    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Result[B] =
      base.foldLeft[(B, Boolean)](
        (initial, false),
        (_, _) match {
          case ((accum, false), element) if element === sentinel => (accum, false).continue
          case ((accum, _), element) => f(accum, element).map(_ -> true)
        }
      ).map(_._1)