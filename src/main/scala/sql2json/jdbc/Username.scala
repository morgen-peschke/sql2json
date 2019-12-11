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

opaque type Username = String
object Username 
  def apply[C[_]](raw: String)(given ApplicativeError[C, Errors]): C[Username] =
    raw.trim.pure[FailFast.Validated]
      .ensure("Username cannot be empty".as[Errors])(_.nonEmpty)
      .as[C[Username]]

  given Show[Username] = identity(_)

  def (props: Properties) username (username: Username): Properties =
    props.put("user", username)
    props