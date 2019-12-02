package sql2json
package config

import types.Validated
import types.Validated.given
import jdbc.{JdbcUrl, Database, Username, Password}
import cat.Show
import Show.given

case class DBConfig(
  jdbcURL: JdbcUrl,
  username: Username,
  password: Password
)
object DBConfig
  given Show[DBConfig] = 
    d => s"DBConfig(jdbc-url: ${d.jdbcURL.show}, userName: ${d.username.show}, password: ${d.password.show})"

case class Config(databases: Map[Database, DBConfig])
  def forDatabase(db: Database): Validated[DBConfig] =
    databases.get(db).fold(s"No config for ${db.show}".invalid)(_.valid)

object Config
  given Show[Config] =
     _.databases
      .toList
      .sortBy(_._1)
      .map {
        case (db, cfg) => db.show -> cfg.show
      }
      .mkString("Config(", ", ", ")")