package sql2json
package config

import com.typesafe.config.{ConfigFactory, ConfigValue, Config => JConfig}
import cat.ApplicativeError, ApplicativeError.given
import cat.Show, Show.show
import cat.Functor.given
import cat.Applicative.{~,given}
import cat.Semigroup, Semigroup.given
import cat.SemigroupK, SemigroupK.given
import cat.Monad.given
import types.validation.Errors, Errors.given
import types.validation.Accumulate, Accumulate.given
import types.validation.FailFast, FailFast.given
import types.Generator, Generator.Result.given
import types.Convertible.given
import types.ConvertibleK.given
import types.Done
import jdbc.{JdbcUrl, Database, Driver, Username, Password}

case class DBConfig(
  jdbcURL: JdbcUrl,
  username: Username,
  password: Password,
  driver: Driver
)
object DBConfig
  given Show[DBConfig] = 
    d => show"DBConfig(jdbc-url: ${d.jdbcURL}, userName: ${d.username}, password: ${d.password})"

  def fromConfigValue[C[_]](path: String, cv: ConfigValue)(given ApplicativeError[C, Errors]): C[DBConfig] =
    val conf = cv.atKey("db")
    val validatedUrl: C[JdbcUrl] = 
      try JdbcUrl[C](conf.getString("db.jdbc-url")) 
      catch
        case ex: Exception => s"Bad config at $path.jdbc-url: ${ex.getMessage}".as[Errors].raise[C, JdbcUrl]
    val validatedUsername: C[Username] =
      try Username[C](conf.getString("db.username")) 
      catch
        case ex: Exception => s"Bad config at $path.username: ${ex.getMessage}".as[Errors].raise[C, Username]
    val validatedPassword: C[Password] =
      try Password[C](conf.getString("db.password")) 
      catch
        case ex: Exception => s"Bad config at $path.password: ${ex.getMessage}".as[Errors].raise[C, Password]
    val validatedDriver: C[Driver] = 
      try Driver[C](conf.getString("db.driver"))
      catch
        case ex: Exception => s"Bad config at $path.driver: ${ex.getMessage}".as[Errors].raise[C, Driver]
    
    (validatedUrl |@| validatedUsername |@| validatedPassword |@| validatedDriver).map {
      case url ~ username ~ password ~ driver => DBConfig(url, username, password, driver)
    }

case class Config(databases: Map[Database, DBConfig])
  def forDatabase[C[_]](db: Database)(given ApplicativeError[C, Errors]): C[DBConfig] =
    databases.get(db).fold(show"No config for $db".as[Errors].raise[C, DBConfig])(_.pure[C])

  def default[C[_]](given ApplicativeError[C, Errors]): C[DBConfig] =
    databases.toList match
      case Nil => "Empty config".as[Errors].raise[C, DBConfig]
      case (_, single) :: Nil => single.pure[C]
      case _ => "Unable to fall back to single config".as[Errors].raise[C, DBConfig]

object Config
  given Show[Config] =
     _.databases
      .toList
      .sortBy(_._1)
      .map {
        case (db, cfg) => db.show -> cfg.show
      }
      .mkString("Config(", ", ", ")")

  def load[C[_]](given AE: ApplicativeError[C, Errors]): C[Config] = 
    
    def entryToScala(entry: java.util.Map.Entry[String, ConfigValue]): Accumulate.Validated[Database ~ DBConfig] = 
      Database[Accumulate.Validated](entry.getKey) |@| DBConfig.fromConfigValue[Accumulate.Validated](entry.getKey, entry.getValue)

    def initFold: Accumulate.Validated[List[(Database, DBConfig)]] = List.empty[(Database, DBConfig)].pure[Accumulate.Validated]
    def squash(accum: Accumulate.Validated[List[(Database, DBConfig)]], element: Accumulate.Validated[(Database, DBConfig)]): Accumulate.Validated[List[(Database, DBConfig)]] = 
      accum.combine(element.map(_.pure[List]))

    val conf = ConfigFactory.load()
    Generator
      .fromStream(conf.getConfig("databases").root().entrySet().stream)
      .map[Accumulate.Validated[Database ~ DBConfig]](entryToScala)
      .toList
      .asKind[FailFast.Validated]
      .flatMap(_.foldLeft(initFold)(squash(_, _)).asKind[FailFast.Validated])
      .map(_.toMap)
      .map(Config(_))
      .asKind[C]
