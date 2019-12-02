package sql2json
package config

import cat.Applicative.~
import cat.Functor.given
import cat.Monad.given
import cat.ApplicativeError.given

import types.{Validated, Done, Errors}
import types.Validated.given
import types.FailFastValidated.given

import java.nio.file.{Path, FileSystem, Files}
import java.io.BufferedReader

import sql2json.jdbc.{JdbcUrl, Database, Username, Password}

import parsing.{ParseState, ParseContext}
import scala.annotation.tailrec

trait ConfigReader 
  def read(path: Path): Validated[Config]

object ConfigReader
  final val JdbcUrlKey = "jdbc-url"
  final val UsernameKey = "username"
  final val PasswordKey = "password"
  final val ConfigFormatRegex = "(^[:]+)\\s(.*)".r

  object DefaultConfigReader extends ConfigReader
    def read(path: Path): Validated[Config] =
      @tailrec
      def loop(br: BufferedReader, state: ParseState)(given ctx: ParseContext): Validated[Config] =
        Option(br.readLine()) match
          case None => state.finish
          case Some(line) =>
            state.consumeLine(line) match 
              case Validated.Valid(nextState) => loop(br, nextState)(given ctx.next)
              case invalidResult => 
                invalidResult
                  .map(_ => Config(Map.empty))
                  .mapError[Errors](_.map(error => s"$error <<<$line>>>"))
      
      Validated
        .catchOnly[java.nio.file.NoSuchFileException](Files.newBufferedReader(path))
        .failFast
        .flatMap(loop(_, ParseState.init)(given ParseContext(path)).failFast)
        .accumulate