package sql2json
package cli

import java.nio.file.{Path,Paths}

import cat.Show
import cat.Show.given
import cat.Functor.given
import cat.Monad.given
import types.Validated
import types.Validated.given
import types.FailFastValidated.given
import config.ConfigReader.DefaultConfigReader
import jdbc.{Database, Sql}
import config.DBConfig

final case class Arguments(dbConfig: DBConfig, sql: Sql)

object Arguments
  given cat.Show[Arguments] = a => s"Arguments(dbConfig: ${a.dbConfig.show}, sql: ${a.sql.show})"

  final val HelpText =
    """|Usage sql2json config-file sql
       |""".stripMargin

  def parse(args: Seq[String]): Validated[Arguments] =
    args match
     case Seq(configPath, dbName, sql) =>
      {
        for 
          database <- Database(dbName).failFast
          config   <- DefaultConfigReader.read(Paths.get(configPath)).failFast
          dbConfig <- config.forDatabase(database).failFast
        yield Arguments(dbConfig, Sql(sql))
      }.accumulate
     case _ => HelpText.invalid