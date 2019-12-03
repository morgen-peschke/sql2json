package sql2json
package jdbc

import types.Validated
import types.Validated.given

opaque type Database = String
object Database
  def apply(raw: String): Validated[Database] =
    raw match 
      case "" => "Database label cannot be empty".invalid
      case junk if junk.startsWith("-") => "Database lable cannot start with '-'".invalid
      case dbName => dbName.valid

  given cat.Show[Database] = identity(_)
  given Ordering[Database] = _ compare _