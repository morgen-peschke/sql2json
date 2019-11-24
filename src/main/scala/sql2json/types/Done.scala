package sql2json
package types

/**
 * Dead simple placeholder to avoid auto-conversion issues with Unit in monadic code.
 */ 
sealed abstract class Done
object Done extends Done
  def upcast: Done = this

  def [A] (a: A) done: Done = Done