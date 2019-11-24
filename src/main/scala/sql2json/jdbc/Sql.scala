package sql2json
package jdbc

import cat.Show
import Show.syntax
import types.{Validated,Done,Generator}
import Generator.Action
import Done.done
import types.json.Json
import types.Credentials.Username.username
import types.Credentials.Password.password
import config.DBConfig
import JdbcUrl.connect
import Row.{asRow, resultSetAsJson}

import java.util.Properties
import java.sql.{Connection, DriverManager, Statement, ResultSet, ResultSetMetaData}

given Show[ResultSet]
  def show (rs: ResultSet): String = rs.toString

/**
 * Very, very minimal wrapper around JDBC.
 *
 * The primary reason this exists is so I don't have to think about mananging
 * the lifecycle of the various JDBC objects.
 */
opaque type Sql = String
object Sql
  def apply(q: String): Sql = q

  given Show[Sql]
    def show (s: Sql): String = s

  def (sql: Sql) query (dbConfig: DBConfig, outputType: OutputType): Generator[Json] = 
    for 
      conn <- connection(dbConfig)
      stmt <- statement(conn)
      rs   <- query(stmt, sql)
      json <- results(rs, outputType)
    yield json

  def connection(dbConfig: DBConfig): Generator[Connection] =
    Generator.ofResource(
      dbConfig.show,
      () => {
        val connection = dbConfig.jdbcURL.connect {
          new Properties()
            .username(dbConfig.username)
            .password(dbConfig.password)
        }
        connection.setAutoCommit(false)
        connection
      },
      _.close().done
    )

  def statement(connection: Connection): Generator[Statement] =
    Generator.ofResource(
      "SqlQuery",
      () => connection.createStatement(),
      _.close().done
    )

  def query(statement: Statement, sql: String): Generator[ResultSet] = 
    Generator.ofResource(
      "ResultSet",
      () => statement.executeQuery(sql),
      _.close().done
    )

  def results(rs: ResultSet, outputType: OutputType): Generator[Json] =
    Generator.unfold(rs) { resultSet => 
      if (resultSet.next())
        Action.Continue(resultSet.asRow.resultSetAsJson(outputType)(given rs.getMetaData))
      else
        Action.Stop(Json.nil)
    }.takeUntil(Json.nil)

    