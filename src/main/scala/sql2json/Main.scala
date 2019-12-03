package sql2json

import java.nio.file.Path

import cat.ApplicativeError.given
import cat.Show
import Show.given
import cli.Arguments
import types.Validated.{Invalid, given}
import types.FailFastValidated.given
import types.Done.given
import types.Generator.Action.given
import jdbc.{OutputType, Sql}
import jdbc.Sql.executeAll
import java.io.{BufferedReader, InputStreamReader}
import java.util.stream.Collector

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
        case Right(config) =>
          val sqlReader = new BufferedReader(new InputStreamReader(System.in))
          try 
            sqlReader.lines.collect(Sql.collector)
            .executeAll(config.dbConfig, config.format)
            .foreach { result =>
              println(result.show).done.continue
            }
            .asValidated match
              case Invalid(errors) =>
                errors.toList.foreach(System.err.println(_))
                System.exit(10)
              case _ =>
                System.exit(0)
          finally
            sqlReader.close()