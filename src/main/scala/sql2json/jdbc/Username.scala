package sql2json
package jdbc

import types.validation.Validated
import types.validation.Validated.given
import cat.Show

import java.util.Properties

opaque type Username = String
object Username 
  def apply(raw: String): Validated[Username] =
    raw.trim match 
      case "" => "Username cannot be empty".invalid
      case trimmed => trimmed.valid

  given Show[Username] = identity(_)

  def (props: Properties) username (username: Username): Properties =
    props.put("user", username)
    props