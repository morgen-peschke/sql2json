package sql2json
package config

import com.typesafe.config.{ConfigFactory, ConfigValue, Config => JConfig}
import types.{Validated, Generator}
import types.Validated.given
import types.FailFastValidated.given
import jdbc.{JdbcUrl, Database, Driver, Username, Password}
import cat.Show
import Show.given
import cat.Functor.given
import cat.Applicative.{~,given}
import cat.Semigroup.given
import cat.Monad.given

case class DBConfig(
  jdbcURL: JdbcUrl,
  username: Username,
  password: Password,
  driver: Driver
)
object DBConfig
  given Show[DBConfig] = 
    d => show"DBConfig(jdbc-url: ${d.jdbcURL}, userName: ${d.username}, password: ${d.password})"

  def fromConfigValue(path: String, cv: ConfigValue): Validated[DBConfig] =
    val conf = cv.atKey("db")
    val validatedUrl: Validated[JdbcUrl] = 
      try JdbcUrl(conf.getString("db.jdbc-url")) 
      catch
        case ex: Exception => s"Bad config at $path.jdbc-url: ${ex.getMessage}".invalid
    val validatedUsername: Validated[Username] =
      try Username(conf.getString("db.username")) 
      catch
        case ex: Exception => s"Bad config at $path.username: ${ex.getMessage}".invalid
    val validatedPassword: Validated[Password] =
      try Password(conf.getString("db.password")) 
      catch
        case ex: Exception => s"Bad config at $path.password: ${ex.getMessage}".invalid
    val validatedDriver: Validated[Driver] = 
      try Driver(conf.getString("db.driver")) 
      catch
        case ex: Exception => s"Bad config at $path.driver: ${ex.getMessage}".invalid[Driver]
    
    (validatedUrl |@| validatedUsername |@| validatedPassword |@| validatedDriver).map {
      case url ~ username ~ password ~ driver => DBConfig(url, username, password, driver)
    }

case class Config(databases: Map[Database, DBConfig])
  def forDatabase(db: Database): Validated[DBConfig] =
    databases.get(db).fold(show"No config for $db".invalid)(_.valid)

  def default: Validated[DBConfig] =
    databases.toList match
      case Nil => "Empty config".invalid[DBConfig]
      case (_, single) :: Nil => single.valid
      case _ => "Unable to fall back to single config".invalid[DBConfig]

object Config
  given Show[Config] =
     _.databases
      .toList
      .sortBy(_._1)
      .map {
        case (db, cfg) => db.show -> cfg.show
      }
      .mkString("Config(", ", ", ")")

  def load: Validated[Config] = 
    val conf = ConfigFactory.load()
    Generator
      .fromStream(conf.getConfig("databases").root().entrySet().stream)
      .map { entry => 
        Database(entry.getKey) |@| DBConfig.fromConfigValue(s"${entry.getKey}", entry.getValue)
      }
      .toList
      .asValidated
      .failFast
      .flatMap(_.foldLeft(List.empty[(Database, DBConfig)].valid)(_ combine _.map(_.pure[List])).failFast)
      .accumulate
      .map(_.toMap)
      .map(Config(_))
