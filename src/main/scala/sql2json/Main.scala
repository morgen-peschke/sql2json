package sql2json

import java.nio.file.Path

import cat.ApplicativeError.given
import cat.Show, Show.show
import cli.Arguments
import types.Convertible, Convertible.given
import types.validation.Accumulate, Accumulate.{Invalid, given}
import types.validation.FailFast, FailFast.given
import types.Done, Done.given
import types.Generator
import types.Generator.Result.given
import types.Generator.Action.given
import jdbc.Sql, Sql.executeAll
import java.io.{BufferedReader, InputStreamReader}
import java.util.stream.Collector

given Show[Path]
  def show(a: Path): String = a.getFileName.toString

object Main 
  @main def run(args: String*): Unit = 
    Arguments
      .parse[Accumulate.Validated](args)
      .toEither match 
        case Left(helpText) => 
          helpText.toList.foreach(System.out.println)
          System.exit(1)
        case Right(config) =>
          val sqlReader = new BufferedReader(new InputStreamReader(System.in))
          try 
            sqlReader.lines.collect(Sql.collector)
              .executeAll()(given config.dbConfig, config.format)
              .foreach { result =>
                println(result.show).done.continue
              }
              .as[Accumulate.Validated[Done]] match
                case Invalid(errors) =>
                  errors.toList.foreach(System.err.println(_))
                  System.exit(10)
                case _ => ()
          finally
            sqlReader.close()