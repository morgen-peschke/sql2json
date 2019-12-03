package sql2json
package jdbc

import types.Validated
import types.Validated.given
import cat.Show

import java.sql.{Connection, DriverManager, Driver}
import java.util.Properties

opaque type JdbcUrl = String
object JdbcUrl
  def apply(raw: String): Validated[JdbcUrl] =
    raw match 
      case "" => "JDBC url cannot be empty".invalid
      case malformed if !malformed.startsWith("jdbc:") => "Expected JDBC url to start with 'jdbc:'".invalid
      case url => url.valid
  
  given Show[JdbcUrl] = identity(_)

  def (driver: Driver) connect (url: JdbcUrl)(props: Properties): Connection =
    driver.connect(url, props)