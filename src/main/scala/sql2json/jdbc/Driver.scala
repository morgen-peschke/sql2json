package sql2json
package jdbc

import types.Validated
import types.Validated.given
import cat.Show

import java.util.Properties
import java.sql.{Connection, Driver => JDriver}


opaque type Driver = Class[?]
object Driver 
  def apply(raw: String): Validated[Driver] =
    raw.trim match 
      case "" => "Password cannot be empty".invalid
      case trimmed => 
        Validated.catchOnly[ClassNotFoundException] {
          Class.forName(trimmed)
        }

  given Show[Driver] = _.getName

  def (driver: Driver) load: JDriver = driver.newInstance.asInstanceOf[JDriver]
