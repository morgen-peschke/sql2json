package sql2json
package jdbc

import types.Validated
import types.Validated.given

opaque type Database = String
object Database
  def apply(raw: String): Validated[Database] =
    raw match 
      case "" => "Database label cannot be empty".invalid
      case dbName => dbName.valid

  given cat.Show[Database]
      def show(db: Database): String = db
    
  given Ordering[Database]
      def compare(a: Database, b: Database) = a.compare(b)