package sql2json
package jdbc

import cat.Show
import cat.Applicative.given
import cat.Monad.given 
import cat.MonadError, MonadError.given
import cat.ApplicativeError, ApplicativeError.given
import types.Convertible.given 
import types.validation.Errors, Errors.given
import types.validation.FailFast, FailFast.Validated.given

import java.util.Properties
import java.sql.{Connection, Driver => JDriver}

opaque type Driver = Class[JDriver]
object Driver 
  def apply[C[_]](raw: String)(given AE: ApplicativeError[C,Errors]): C[Driver] =
    raw.trim.pure[FailFast.Validated]
      .ensure("Driver class cannot be empty".as[Errors])(_.nonEmpty)
      .flatMap { trimmed =>
        AE.catchOnly[ClassNotFoundException](Class.forName(trimmed)).as[FailFast.Validated[Class[?]]]
      }
      .flatMap { untypedClass => 
        AE.catchOnly[ClassCastException](untypedClass.asInstanceOf[Class[JDriver]]).as[FailFast.Validated[Class[JDriver]]]
      }
      .as[C[Driver]]
   
  given Show[Driver] = _.getName

  def (driver: Driver) load: JDriver = driver.newInstance
