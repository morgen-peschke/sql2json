package sql2json
package jdbc

import cat.Show
import cat.SemigroupK.given
import Show.given
import types.{Validated,Done,Generator}
import Generator.Action.{halt,given}
import Done.given
import types.json.Json
import Username.username
import Password.password
import config.DBConfig
import JdbcUrl.connect
import Driver.load
import Row.{asRow, resultSetAsJson, metaDataAsJson}

import java.util.Properties
import java.util.stream.Collector
import java.sql.{Connection, DriverManager, Statement, ResultSet, ResultSetMetaData, SQLException}

given Show[ResultSet] = _.toString

/**
 * Very, very minimal wrapper around JDBC.
 *
 * The primary reason this exists is so I don't have to think about mananging
 * the lifecycle of the various JDBC objects.
 */
opaque type Sql = String
object Sql
  def apply(q: String): Sql = q

  given Show[Sql] = identity(_)

  class SqlCollectorState(var buffer: List[Sql], var builder: StringBuilder)
    override def toString(): String = s"SqlCollectorState($buffer, $builder)"

    def append(line: String): Unit = {
      line match
        case ";" if builder.isEmpty => ()
        case ";" => 
          buffer = Sql(builder.toString) :: buffer
          builder = new StringBuilder
        case notDelim => 
          builder.append(notDelim).append(' ')
    }

    def combine(other: SqlCollectorState): SqlCollectorState = 
      throw new IllegalStateException("Combining SqlCollectorState is undefined")

    def finish: List[Sql] = (Sql(builder.toString) :: buffer).reverse

  object SqlCollectorState
    def init: SqlCollectorState = SqlCollectorState(Nil, new StringBuilder)

  val collector: Collector[String, SqlCollectorState, List[Sql]] = 
    Collector.of[String, SqlCollectorState, List[Sql]](
      () => SqlCollectorState.init,
      _ append _,
      _ combine _,
      _.finish
    )

  def (statements: List[Sql]) executeAll (dbConfig: DBConfig, outputType: OutputType): Generator[Json] =
    def loop(connection: Connection, remaining: List[Sql]): Generator[Json] =
      remaining match
        case Nil => Generator.empty[Json]
        case last :: Nil => 
          for 
            stmt <- statement(connection)
            rs   <- query(stmt, last)
            json <- columnInfo(rs, outputType).combineK(results(rs, outputType))
          yield json
        case notLast :: rest => 
          val genResult = 
            for
              stmt <- statement(connection)
            yield 
              try stmt.execute(notLast)
              catch 
                case ex: Exception => throw new RuntimeException(s"Failed to execute: $notLast", ex)
              Json.nil

          genResult combineK loop(connection, rest)
        
    connection(dbConfig).flatMap(loop(_, statements)).dropWhile(Json.nil)
      

  def (sql: Sql) query (dbConfig: DBConfig, outputType: OutputType): Generator[Json] = 
    for 
      conn <- connection(dbConfig)
      stmt <- statement(conn)
      rs   <- query(stmt, sql)
      json <- columnInfo(rs, outputType).combineK(results(rs, outputType))
    yield json

  def connection(dbConfig: DBConfig): Generator[Connection] =
    Generator.ofResource(
      dbConfig.show,
      () => {
        val connection = dbConfig.driver.load.connect(dbConfig.jdbcURL) {
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

  def execute(statement: Statement, sql: String): Validated[Done] = 
    Validated.catchOnly[SQLException](statement.execute(sql).done)

  def query(statement: Statement, sql: String): Generator[ResultSet] = 
    Generator.ofResource(
      "Query:ResultSet",
      () => {
        try statement.executeQuery(sql)
        catch 
          case ex: Exception => throw new RuntimeException(s"Failed to run: $sql", ex)
      },
      _.close().done
    )

  def columnInfo(rs: ResultSet, outputType: OutputType): Generator[Json] =
    outputType match
      case OutputType.ArrayWithHeader(verbose) =>
        Generator.one[Json](rs.asRow.metaDataAsJson(rs.getMetaData, verbose))
      case _ => Generator.empty[Json]

  def results(rs: ResultSet, outputType: OutputType): Generator[Json] =
    Generator.unfold(rs, resultSet => {
      if resultSet.next()
      then resultSet.asRow.resultSetAsJson(outputType)(given rs.getMetaData).continue
      else halt
    })

    