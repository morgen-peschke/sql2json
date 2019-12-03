package sql2json
package cli

import java.nio.file.{Path,Paths}

import cat.Show.given
import cat.Functor.given
import cat.Monad.given
import cat.ApplicativeError.given
import types.{Validated, FailFastValidated, NonEmptyList}
import types.Validated.given
import types.FailFastValidated.given
import config.Config
import config.ConfigReader.DefaultConfigReader
import jdbc.{Database, Sql, OutputType}
import config.DBConfig

final case class Arguments(dbConfig: DBConfig, format: OutputType)

object Arguments
  given cat.Show[Arguments] = a => s"Arguments(dbConfig: ${a.dbConfig.show}, format: ${a.format.show})"

  private final val ArrayFmt: String = "array"
  private final val ObjectFmt: String = "object"

  final val HelpText =
    s"""|Usage: sql2json config-file [options]
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
        |""".stripMargin

  private def processArgs(config: Config, 
                          rest: List[String], 
                          dbNameOpt: Option[Database] = None,
                          formatAsObject: Boolean = false, 
                          omitHeader: Boolean = true, 
                          verbose: Boolean = false): FailFastValidated[Arguments] =
    rest match 
      case ("-h" | "--header") :: remaining => processArgs(config, remaining, dbNameOpt, formatAsObject, false, verbose)
      case ("-v" | "--verbose") :: remaining => processArgs(config, remaining, dbNameOpt, formatAsObject, omitHeader, true)

      case arg @ ("-f" | "--format") :: paramAndRemaining => paramAndRemaining match
        case Nil => s"Missing <fmt> after $arg".invalid.failFast
        case ArrayFmt :: remaining => processArgs(config, remaining, dbNameOpt, false, omitHeader, verbose)
        case ObjectFmt :: remaining => processArgs(config, remaining, dbNameOpt, true, omitHeader, verbose)
        case junk :: _ => s"Unrecognized <fmt> after $arg (expected '$ObjectFmt' or '$ArrayFmt'): $junk".invalid.failFast

      case arg @ ("-d" | "--database") :: paramAndRemaining => paramAndRemaining match
        case Nil => s"Missing <name> after $arg".invalid.failFast
        case rawName :: remaining => 
          Database(rawName).toEither match
          case Right(dbName) => processArgs(config, remaining, Some(dbName), formatAsObject, omitHeader, verbose)
          case Left(errors) => errors.map(e => s"Invalid <name> after $arg: $e").raise[Validated,Arguments].failFast

      case Nil =>
        val validatedDbConfig = 
          dbNameOpt match 
            case Some(dbName) => 
              config.databases.get(dbName).fold(s"No config for database ${dbName.show}".invalid[DBConfig])(_.valid)
            case None => 
              config.databases.toList match
                case Nil => "Empty config".invalid[DBConfig]
                case (_, single) :: Nil => single.valid
                case _ => "Unable to fall back to single config".invalid[DBConfig]

        validatedDbConfig.map { dbConfig => 
          Arguments(
            dbConfig,
            if formatAsObject then OutputType.Object
            else if omitHeader then OutputType.BareArray
            else OutputType.ArrayWithHeader(verbose)
          )
        }.failFast

      case junk => s"Unrecognized argument, starting at: ${junk.show}".invalid.failFast


  def parse(args: Seq[String]): Validated[Arguments] =
    args.toList match
      case Nil => NonEmptyList.of("No arguments", HelpText).raise[Validated, Arguments]
      case configPath :: rest =>
        DefaultConfigReader.read(Paths.get(configPath)).failFast.flatMap(processArgs(_, rest)).accumulate
