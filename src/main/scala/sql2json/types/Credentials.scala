package sql2json
package types

import types.Validated
import types.Validated.given
import cat.Show

import java.util.Properties

object Credentials 
  opaque type Username = String
  object Username 
    def apply(raw: String): Validated[Username] =
      raw.trim match 
        case "" => "Username cannot be empty".invalid
        case trimmed => trimmed.valid

    given Show[Username]
      def show(un: Username): String = un

    def (props: Properties) username (username: Username): Properties =
      props.put("user", username)
      props

  opaque type Password = String
  object Password 
    def apply(raw: String): Validated[Password] =
      raw.trim match 
        case "" => "Password cannot be empty".invalid
        case trimmed => trimmed.valid

    given Show[Password]
      def show(p: Password): String = "********"

    def (props: Properties) password (password: Password): Properties =
      props.put("password", password)
      props