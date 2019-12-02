package sql2json
package jdbc

import types.Validated
import types.Validated.given
import cat.Show

import java.util.Properties

opaque type Password = String
object Password 
  def apply(raw: String): Validated[Password] =
    raw.trim match 
      case "" => "Password cannot be empty".invalid
      case trimmed => trimmed.valid

  given Show[Password] = _ => "********"

  def (props: Properties) password (password: Password): Properties =
    props.put("password", password)
    props