package sql2json
package jdbc

import cat.Show
import cat.Applicative.given
import cat.MonadError, MonadError.given
import cat.ApplicativeError, ApplicativeError.given
import types.Convertible.given 
import types.validation.Errors, Errors.given
import types.validation.FailFast, FailFast.Validated.given

import java.sql.{Connection, DriverManager, Driver}
import java.util.Properties

opaque type JdbcUrl = String
object JdbcUrl
  def apply[C[_]](raw: String)(given ApplicativeError[C, Errors]): C[JdbcUrl] =
    raw.trim.pure[FailFast.Validated]
      .ensure("JDBC url cannot be empty".as[Errors])(_.nonEmpty)
      .ensure("Expected JDBC url to start with 'jdbc:'".as[Errors])(_.startsWith("jdbc:"))
      .as[C[JdbcUrl]]
  
  given Show[JdbcUrl] = identity(_)

  given ops: AnyRef 
    def (driver: Driver) connect (url: JdbcUrl)(props: Properties): Connection =
      driver.connect(url, props)