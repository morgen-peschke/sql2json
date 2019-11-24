package sql2json
package types

import cat.{Applicative, Functor, Monoid, MonoidK, Show}
import cat.Functor.given
import cat.Semigroup.given
import cat.SemigroupK.given
import cat.Show.given
import Done.done
import Generator.{Action, given}
import Generator.Action.given

import scala.reflect.ClassTag
import scala.annotation.tailrec

/**
 * The idea for this was shamelessly ripped off of [Li Haoyi's `geny`](https://github.com/lihaoyi/geny).
 * 
 * The implementation is my own, so don't judge his code by this (particularly as I think it's got a bug in it).
 */
trait Generator[A]
  def foldLeft[B](initial: B, f: (B, A) => Action[B]): Action[B]

  def foreach(f: A => Action[Done]): Done = foldLeft(Done.upcast, (_, v) => f(v).map(_ => Done)).result
  
  def fold(given Monoid[A]): A = 
    foldLeft(
      Monoid[A].empty, 
      (accum, v) => Action.Continue(accum combine v)
    ).result

  def foldK[C[_]: Applicative: MonoidK]: C[A] = 
    foldLeft[C[A]](
      MonoidK[C].emptyK, 
      (accum, v) => Action.Continue(accum combineK Applicative[C].pure(v))
    ).result

  def map[B](f: A => B): Generator[B] = new Generator.Map(f, this)
  def flatMap[B](f: A => Generator[B]): Generator[B] = new Generator.FlatMap(f, this)
  def take(count: Long): Generator[A] = new Generator.Take(count, this)
  def takeWhile(p: A => Boolean): Generator[A] = new Generator.While(p, this)
  def takeUntil(sentinel: A)(given Show[A]): Generator[A] = new Generator.Until(sentinel, this)

object Generator
  sealed abstract class Action[A](val result: A)
    def stop: Action[A] =
      this match
        case Action.Continue(r) => Action.Stop(r)
        case r @ Action.Stop(_) => r

  object Action
    // Keep doing what you're doing
    case class Continue[A](override val result: A) extends Action[A](result)
    // Stop the generator
    case class Stop[A](override val result: A) extends Action[A](result)

    given Functor[Action]
      def map [A,B] (fa: Action[A], f: A => B): Action[B] = 
        fa match 
          case Continue(a) => Continue(f(a))
          case Stop(a) => Stop(f(a))

    trait ActionOps[A]
      def (a: A) continue: Continue[A] = Continue(a)
      def (a: A) stop: Stop[A] = Stop(a)

    given[A]: ActionOps[A]

  def empty[A]: Generator[A] = new Empty[A]
  def one[A: Show](value: A): Generator[A] = new One(value)
  def const[A: Show](value: A): Generator[A] = new Const(value)
  def continually[A](value: => A): Generator[A] = new Continually(value)
  def calculate[A: Show](seed: A)(step: A => Action[A]): Generator[A] = new Calculate(seed, step)
  def ofResource[A](name: String, aquire: () => A, cleanup: A => Done): Generator[A] = new OfResource(name, aquire, cleanup)
  def unfold[A: Show,B](seed: A)(expand: A => Action[B]): Generator[B] = new Unfold(seed, expand)

  class Empty[A] extends Generator[A]
    override def toString: String = "Generator.empty"
    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Action[B] = initial.stop
    override def foreach(f: A => Action[Done]): Done = Done
    override def fold(given Monoid[A]): A = Monoid[A].empty
    override def foldK[C[_]: Applicative: MonoidK]: C[A] = MonoidK[C].emptyK[A]

  class One[A: Show](value: A) extends Generator[A]
    override def toString: String = s"Generator.one(${value.show})"
    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Action[B] = f(initial, value).stop
    override def foreach(f: A => Action[Done]): Done = f(value).done

    override def fold(given Monoid[A]): A = value
    override def foldK[C[_]: Applicative: MonoidK]: C[A] = Applicative[C].pure(value)

  class Const[A: Show](value: A) extends Generator[A]
    override def toString: String = s"Generator.const(${value.show})"
    
    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Action[B] =
      @tailrec
      def loop(accum: B): Action[B] = 
        f(accum, value) match
          case Action.Continue(result) => loop(accum)
          case Action.Stop(result) => accum.stop 
      
      loop(initial)
    
  class Continually[A](value: => A) extends Generator[A]
    override def toString: String = "Generator.continually(???)"
    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Action[B] =
      @tailrec
      def loop(accum: B): Action[B] = 
        f(accum, value) match
          case Action.Continue(result) => loop(accum)
          case Action.Stop(result) => accum.stop
        
      loop(initial)

  class Calculate[A: Show](seed: A, step: A => Action[A]) extends Generator[A]
    override def toString: String = 
      s"Generator.calculate(${seed.show})($step)"

    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Action[B] =
      @tailrec
      def loop(accum: B, prev: A): Action[B] = 
        step(prev) match
          case Action.Stop(next) => f(accum, next).stop
          case Action.Continue(next) => f(accum, next) match
            case Action.Continue(result) => loop(accum, next)
            case result @ Action.Stop(_) => result
          
      loop(initial, seed)

  class OfResource[A](name: String, aquire: () => A, cleanup: A => Done) extends Generator[A]
    override def toString: String = s"Generator.ofResource($name)"
    
    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Action[B] =
      val a = aquire()
      try f(initial, a).stop
      finally cleanup(a)

  class Unfold[A: Show,B](seed: A, expand: A => Action[B]) extends Generator[B]
    override def toString: String = s"Generator.unfold(${seed.show})($expand)"

    def foldLeft[C](initial: C, f: (C, B) => Action[C]): Action[C] =
      @tailrec
      def loop(accum: C): Action[C] = 
        expand(seed) match
          case Action.Stop(next) => f(accum, next).stop
          case Action.Continue(next) => f(accum, next) match
            case Action.Continue(result) => loop(accum)
            case result @ Action.Stop(_) => result
            
      loop(initial)

  class Map[A,B](fab: A => B, base: Generator[A]) extends Generator[B]
    override def toString: String = s"$base.map($fab)"
    
    def foldLeft[C](initial: C, fcb: (C, B) => Action[C]): Action[C] = 
      base.foldLeft(initial, (accum, value) => fcb(accum, fab(value)))
     
  class FlatMap[A,B](fab: A => Generator[B], base: Generator[A]) extends Generator[B]
    override def toString: String = s"$base.flatMap($fab)"

    def foldLeft[C](initial: C, fcb: (C, B) => Action[C]): Action[C] = 
      base.foldLeft(
        initial, 
        (outerAccum, outerValue) => 
          fab(outerValue).foldLeft(
            outerAccum,
            (innerAccum, innerValue) =>
              fcb(innerAccum, innerValue)
          )
      )
  
  class Take[A](amount: Long, base: Generator[A]) extends Generator[A]
    override def toString: String = s"$base.take($amount)"

    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Action[B] =
      base.foldLeft(
        (initial, amount), 
        (accumAndCount, value) => {
          accumAndCount match 
            case result @ (_, 0L) => result.stop
            case (accum, remaining) => 
              f(accum, value) match 
                case Action.Continue(result) => (result, remaining - 1L).continue
                case Action.Stop(result) => (result, 0L).stop
        }).map(_._1)

  class While[A](predicate: A => Boolean, base: Generator[A]) extends Generator[A]
    override def toString: String = s"$base.while($predicate)"

    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Action[B] =
      base.foldLeft(
        initial, 
        (accum, value) => {
          if (predicate(value)) then f(accum, value)
          else accum.stop
        })
    
  class Until[A: Show](sentinel: A, base: Generator[A]) extends Generator[A]
    override def toString: String = s"$base.until(${sentinel.show})"

    def foldLeft[B](initial: B, f: (B, A) => Action[B]): Action[B] =
      base.foldLeft(
        initial, 
        (accum, value) => {
          if (value == sentinel) then f(accum, value)
          else accum.stop
        })