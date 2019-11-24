package sql2json
package types
package json

import cat.Show
import Show.syntax

/**
 * Very simple JSON representation.
 *
 * Currently supports serialization of arbitrary types to [[Json]] using the [[Serialize]]
 * typeclass, and serialization of [[Json]] to [[scala.String]] (sorry, no pretty-printing).
 *
 * Does not support parsing or mapping [[Json]] to arbitrary types, as that facility isn't 
 * needed for this project.
 */
enum Json
  case Nil
  case Number(value: BigDecimal)
  case Bool(value: Boolean)
  case Text(value: String)
  case Object(fields: Map[String, Json])
  case Array(elements: Vector[Json])

object Json
  def obj(fields: (String, Json)*): Json = Json.Object(fields.toMap)
  def arr(elements: Json*): Json = Json.Array(elements.toVector)
  def nil: Json = Json.Nil

  private def (c: Char) escaped: String = s"\\$c"

  private val EscapingMap: Map[Char, String] = Map(
    '\\' -> '\\'.escaped,
    '"'  -> '"'.escaped,
    '\b' -> 'b'.escaped,
    '\t' -> 't'.escaped,
    '\n' -> 'n'.escaped,
    '\f' -> 'f'.escaped,
    '\r' -> 'r'.escaped
  )

  // Thank you Stack Overflow: https://stackoverflow.com/a/16652683/1188897
  def (value: String) escaped: String = 
    value
      .foldLeft(new StringBuilder(value.length).append('"')) { 
        (builder, char) =>
          EscapingMap.get(char) match
            case Some(escapedValue) => builder.append(escapedValue)
            case None if char < ' ' => builder.append(s"\\u000${char.toInt.toHexString}".takeRight(4))
            case None               => builder.append(char)
      }
      .append('"')
      .toString

  given Show[Json]
    def show(j: Json): String = j match
      case Json.Nil => "null"
      case Json.Bool(true) => "true"
      case Json.Bool(false) => "false"
      case Json.Number(value) => value.toString
      case Json.Text(value) => value.escaped
      case Json.Array(elements) => elements.map(_.show).mkString("[", ",", "]")
      case Json.Object(fields) => 
        fields.map { 
          case (k, v) => s"${k.escaped}: ${v.show}"
        }
        .mkString("{", ",", "}")