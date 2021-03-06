package sql2json
package cli

import java.nio.file.{Path,Paths}

import cat.Show, Show.show
import cat.Functor.given
import cat.Monad.given
import cat.Applicative.given
import cat.ApplicativeError, ApplicativeError.given
import cat.SemigroupK.given
import types.Convertible.given
import types.validation.Accumulate, Accumulate.given
import types.validation.FailFast, FailFast.given
import types.validation.Errors, Errors.given 
import types.NonEmptyList
import config.Config
import jdbc.{Database, Sql}
import jdbc.SqlResult.OutputType
import config.DBConfig

final case class Arguments(dbConfig: DBConfig, format: OutputType)
object Arguments
  object ShowHelpText
  given Show[Arguments] = a => show"Arguments(dbConfig: ${a.dbConfig}, format: ${a.format})"

  private final val ArrayFmt: String = "array"
  private final val ObjectFmt: String = "object"

  final val HelpText =
    s"""|Usage: sql2json [options]
        |
        | Runs a SQL expression and returns it as lines of JSON. 
        |
        | The query is read from standard input. Because I was not about to write anything close to a SQL 
        | parser, if multiple statements are needed, they must be separate by a line which contains exactly
        | a single ';'.
        |
        | All but the last statement are assumed to be commands, and their result sets are ignored. The final
        | statement is assumed to be a query, and it's result set is processed and returned. If the final 
        | statement is not a query, you are using the wrong tool.
        |
        | Configuration of credentials & dependecies is handled via HOCON config files.
        |
        | Options
        |   -d --database  name   Selects a database from the config file
        |                         If omitted, and the config only contains a single entry, it'll chose that.
        |                         Otherwise, you'll get an error.
        |
        |   -f --format    fmt    Selects the output format, can be '$ObjectFmt' or '$ArrayFmt' (the default)
        |
        |   -h --header           Enables emitting a leading header in '$ArrayFmt' format (disabled by default)
        |
        |   -v --verbose          Enables extra information in the header, when enabled (disabled by default)
        |
        |   --help                Print this help message, then exit               
        |""".stripMargin

  private def processArgs(config: Config, 
                          rest: List[String], 
                          dbNameOpt: Option[Database] = None,
                          formatAsObject: Boolean = false, 
                          omitHeader: Boolean = true, 
                          verbose: Boolean = false): FailFast.Validated[Either[ShowHelpText.type,Arguments]] =
    rest match 
      case "--help" :: _ => Left(ShowHelpText).validFF
      case ("-h" | "--header") :: remaining => processArgs(config, remaining, dbNameOpt, formatAsObject, false, verbose)
      case ("-v" | "--verbose") :: remaining => processArgs(config, remaining, dbNameOpt, formatAsObject, omitHeader, true)

      case arg @ ("-f" | "--format") :: paramAndRemaining => paramAndRemaining match
        case Nil => s"Missing <fmt> after $arg".invalidFF
        case ArrayFmt :: remaining => processArgs(config, remaining, dbNameOpt, false, omitHeader, verbose)
        case ObjectFmt :: remaining => processArgs(config, remaining, dbNameOpt, true, omitHeader, verbose)
        case junk :: _ => s"Unrecognized <fmt> after $arg (expected '$ObjectFmt' or '$ArrayFmt'): $junk".invalidFF[Either[ShowHelpText.type,Arguments]]

      case arg @ ("-d" | "--database") :: paramAndRemaining => paramAndRemaining match
        case Nil => s"Missing <name> after $arg".invalidFF
        case rawName :: remaining => 
          Database[FailFast.Validated](rawName).toEither match
          case Right(dbName) => processArgs(config, remaining, Some(dbName), formatAsObject, omitHeader, verbose)
          case Left(errors) => errors.map(e => s"Invalid <name> after $arg: $e").invalidFF[Either[ShowHelpText.type,Arguments]]

      case Nil =>
        dbNameOpt
          .fold(config.default[FailFast.Validated])(config.forDatabase[FailFast.Validated])
          .map { dbConfig => 
            Right(Arguments(
              dbConfig,
              if formatAsObject then OutputType.Object
              else if omitHeader then OutputType.BareArray
              else OutputType.ArrayWithHeader(verbose)
            ))
          }

      case junk => show"Unrecognized argument, starting at: $junk".invalidFF


  def parse[C[_]](args: Seq[String])(given ApplicativeError[C, Errors]): C[Arguments] =
    val results = args.toList match
      case Nil => "No arguments".as[Errors].raise[FailFast.Validated, Either[ShowHelpText.type,Arguments]]
      case rest => Config.load[C].as[FailFast.Validated[Config]].flatMap(processArgs(_, rest))

    results
      .mapError[Errors](_ combineK HelpText.pure[NonEmptyList])
      .flatMap {
        case Left(ShowHelpText) => HelpText.as[Errors].raise[FailFast.Validated, Arguments]
        case Right(args) => args.pure[FailFast.Validated]
      }
      .as[C[Arguments]]
