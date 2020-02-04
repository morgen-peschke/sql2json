package sql2json
package types

import cat.{Eq, Empty, Semigroup, Monoid, Show}

/**
 * Dead simple placeholder to avoid auto-conversion issues with Unit in monadic code.
 */ 
sealed abstract class Done
object Done extends Done
  def upcast: Done = this

  trait DoneOps
    def[A] (a: A) done: Done = Done

  given DoneOps

  given Show[Done] = _ => "Done"
  given Eq[Done] = _ == _
  given Empty[Done] = Empty.instance(Done.upcast)
  given Semigroup[Done] = (a, _) => a
  given Monoid[Done] = Monoid.instance[Done]