package sql2json
package jdbc

import cat.ApplicativeError
import cat.SemigroupK.given
import cat.Show, Show.show
import types.validation.Errors, Errors.given
import types.Generator, Generator.Action.{halt,given}
import types.Done, Done.given
import types.Convertible.given
import types.Json
import Username.given
import Password.given
import config.DBConfig
import JdbcUrl.given
import Driver.load
import SqlResult.{OutputType, Row, given}

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

  def (statements: List[Sql]) executeAll() (given DBConfig, OutputType): Generator[Json] =
    def loop(connection: Connection, remaining: List[Sql]): Generator[Json] =
      remaining match
        case Nil => Generator.empty[Json]
        case last :: Nil => 
          for 
            stmt <- statement(connection)
            rs   <- query(stmt, last)
            json <- columnInfo(rs).combineK(results(rs))
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
        
    connection().flatMap(loop(_, statements)).dropWhile(Json.nil)
      

  def (sql: Sql) query ()(given DBConfig, OutputType): Generator[Json] = 
    for 
      conn <- connection()
      stmt <- statement(conn)
      rs   <- query(stmt, sql)
      json <- columnInfo(rs).combineK(results(rs))
    yield json

  def connection()(given dbConfig: DBConfig): Generator[Connection] =
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

  def execute[C[_]](statement: Statement, sql: String)(given AE: ApplicativeError[C, Errors]): C[Done] = 
    AE.catchOnly[SQLException](statement.execute(sql).done)

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

  def columnInfo(rs: ResultSet)(given outputType: OutputType): Generator[Json] =
    outputType match
      case ot @ OutputType.ArrayWithHeader(_) =>
        Generator.one[Json](rs.header.as[Json])
      case _ => Generator.empty[Json]

  def results(rs: ResultSet)(given OutputType): Generator[Json] =
    Generator.unfold(rs, resultSet => {
      if resultSet.next()
      then 
        given ResultSetMetaData = rs.getMetaData
        resultSet.row.as[Json].continue
      else halt
    })

    