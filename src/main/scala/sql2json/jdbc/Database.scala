package sql2json
package jdbc

import cat.Applicative.given
import cat.MonadError, MonadError.given
import cat.ApplicativeError, ApplicativeError.given
import types.Convertible.given 
import types.validation.Errors, Errors.given
import types.validation.FailFast, FailFast.Validated.given

opaque type Database = String
object Database
  def apply[C[_]](raw: String)(given AE: ApplicativeError[C, Errors]): C[Database] =
    raw.trim.pure[FailFast.Validated]
      .ensure("Database label cannot be empty".as[Errors])(_.nonEmpty)
      .prevent("Database lable cannot start with '-'".as[Errors])(_.startsWith("_"))
      .as[C[Database]]

  given cat.Show[Database] = identity(_)
  given Ordering[Database] = _ compare _