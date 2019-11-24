package sql2json

import java.nio.file.Path

import cat.ApplicativeError.given
import cat.Show
import Show.given
import config.ConfigReader.DefaultConfigReader
import cli.Arguments
import types.Validated.given
import types.FailFastValidated.given
import types.Done.done
import types.Generator.Action.given
import jdbc.OutputType
import jdbc.Sql.query

given Show[Path]
  def show(a: Path): String = a.getFileName.toString

object Main 
  @main def run(args: String*): Unit = 
    Arguments
      .parse(args)
      .toEither match 
        case Left(helpText) => 
          helpText.toList.foreach(System.out.println)
          System.exit(1)
        case Right(result) =>
          result
            .sql
            .query(result.dbConfig, OutputType.Object)
            .foreach { result =>
              println(result.show).done.continue
            }
