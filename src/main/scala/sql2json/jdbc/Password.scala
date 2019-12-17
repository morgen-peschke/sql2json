package sql2json
package jdbc

import cat.Show
import cat.Applicative.given
import cat.MonadError, MonadError.given
import cat.ApplicativeError, ApplicativeError.given
import types.Convertible.given 
import types.validation.Errors, Errors.given
import types.validation.FailFast, FailFast.Validated.given

import java.util.Properties

opaque type Password = String
object Password 
  def apply[C[_]](raw: String)(given ApplicativeError[C, Errors]): C[Password] =
    raw.trim.pure[FailFast.Validated]
      .ensure("Password cannot be empty".as[Errors])(_.nonEmpty)
      .as[C[Password]]

  given Show[Password] = _ => "********"

  given ops: AnyRef
    def (props: Properties) password (password: Password): Properties =
      props.put("password", password)
      props