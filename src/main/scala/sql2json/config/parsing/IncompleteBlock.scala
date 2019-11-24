package sql2json
package config
package parsing

import scala.util.matching.Regex

import cat.Applicative.~
import cat.Applicative.given
import cat.ApplicativeError.given
import cat.Functor.given
import cat.Monad.given
import cat.Show.given

import types.Errors
import types.Validated
import types.Validated.given
import types.FailFastValidated.given

import sql2json.types.Credentials.{Username, Password}
import sql2json.jdbc.{JdbcUrl, Database}

import IncompleteBlock.{
  missingKeyErrorMessage,
  doubleAssignmentErrorMessage, 
  JdbcUrlKey, 
  UsernameKey, 
  PasswordKey
}

case class IncompleteBlock(
    dbName: Database,
    blockStarted: ParseContext,
    jdbcURLOpt: Option[(JdbcUrl, ParseContext)], 
    usernameOpt: Option[(Username, ParseContext)], 
    passwordOpt: Option[(Password, ParseContext)])

    def setJdbcURL(url: JdbcUrl)(given PC: ParseContext): Validated[IncompleteBlock] = 
      jdbcURLOpt match
        case None => copy(jdbcURLOpt = Some(url -> PC)).valid
        case Some((_, oldCtx)) => doubleAssignmentErrorMessage(JdbcUrlKey, oldCtx).invalid

    def setUsername(name: Username)(given PC: ParseContext): Validated[IncompleteBlock] =
      usernameOpt match
        case None => copy(usernameOpt = Some(name -> PC)).valid
        case Some((_, oldCtx)) => doubleAssignmentErrorMessage(UsernameKey, oldCtx).invalid

    def setPassword(pwd: Password)(given PC: ParseContext): Validated[IncompleteBlock] =
      passwordOpt match
        case None => copy(passwordOpt = Some(pwd -> PC)).valid
        case Some((_, oldCtx)) => doubleAssignmentErrorMessage(PasswordKey, oldCtx).invalid

    def hasUrl: Validated[JdbcUrl] = jdbcURLOpt.fold(missingKeyErrorMessage(JdbcUrlKey).invalid)(_._1.valid)
    def hasUsername: Validated[Username] = usernameOpt.fold(missingKeyErrorMessage(UsernameKey).invalid)(_._1.valid)
    def hasPassword: Validated[Password] = passwordOpt.fold(missingKeyErrorMessage(PasswordKey).invalid)(_._1.valid)

    def[A] (result: Validated[A]) addContext (given ctx: ParseContext): Validated[A] = 
      result.mapError[Errors](_.map { error => 
        s"${ctx.show} in block for ${dbName.show} (starting at ${blockStarted.show}): $error"
      })

    def complete(given ParseContext): Validated[(Database, DBConfig)] = 
      (hasUrl |@| hasUsername |@| hasPassword)
        .map {
          case url ~ username ~ password => dbName -> DBConfig(url, username, password)
        }
        .addContext

    def consumeLine(line: String)(given ParseContext): Validated[IncompleteBlock] = 
      def ensureHasKey(trimmed: String) = 
        trimmed.splitAt(trimmed.indexOf(':')) match
          case ("", _) => "no key found".invalid 
          case (key, value) => (key.trim, value.drop(1).trim).valid

      def ensureHasValue(value: String) = 
        value.trim match
          case "" => "value is missing".invalid
          case trimmed => trimmed.valid
      
      def consumeKey(key: String, value: String) = 
        key match
          case JdbcUrlKey => JdbcUrl(value).failFast >=> (setJdbcURL(_: JdbcUrl).failFast)
          case UsernameKey => Username(value).failFast >=> (setUsername(_: Username).failFast)
          case PasswordKey => Password(value).failFast >=> (setPassword(_: Password).failFast)
          case _ => "unknown key".invalid.failFast

      val ff = for
        keyVal   <- ensureHasKey(line.trim).failFast
        value    <- ensureHasValue(keyVal._2).failFast
        newState <- consumeKey(keyVal._1, value)
      yield newState
      
      ff.accumulate.addContext

object IncompleteBlock
  final val JdbcUrlKey = "jdbc-url"
  final val UsernameKey = "username"
  final val PasswordKey = "password"

  def init(dbName: Database)(given ctx: ParseContext): IncompleteBlock = IncompleteBlock(dbName, ctx, None, None, None)

  def missingKeyErrorMessage(keyName: String): String = 
    s"incomplete config: missing key $keyName"

  def doubleAssignmentErrorMessage(key: String, oldCtx: ParseContext): String = 
    s"double assignment of $key, previous assignment was at ${oldCtx.show}"