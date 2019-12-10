package sql2json
package jdbc

import types.validation.{Validated, FailFastValidated, Errors}
import Validated.given
import FailFastValidated.given
import Errors.given
import types.Convertible.given
import cat.Applicative.given
import cat.MonadError
import cat.MonadError.given
import cat.Monad.given
import cat.Show

import java.util.Properties
import java.sql.{Connection, Driver => JDriver}

opaque type Driver = Class[JDriver]
object Driver 
  def apply[C[_]](raw: String)(given ME: MonadError[C,Errors]): C[Driver] =
    raw.trim.pure[C].ensure("Driver class cannot be empty".as[Errors])(_.nonEmpty)
      .flatMap { trimmed =>
        ME.applicativeError.catchOnly[ClassNotFoundException] {
          Class.forName(trimmed).asInstanceOf[Class[JDriver]]
        }
      }
   
  given Show[Driver] = _.getName

  def (driver: Driver) load: JDriver = driver.newInstance
