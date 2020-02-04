package sql2json
package jdbc

import cat.Show
import java.sql.{ResultSet,ResultSetMetaData,Types}
import types.Json
import types.Convertible, Convertible.given

object SqlResult
  enum OutputType 
    case BareArray 
    case ArrayWithHeader(verbose: Boolean)
    case Object

  object OutputType
    given Show[OutputType] = _ match
      case Object => "object"
      case BareArray => "array"
      case ArrayWithHeader(true) => "array with fancy header"
      case ArrayWithHeader(false) => "array with header"

  opaque type Header = ResultSetMetaData
  opaque type Row = ResultSet

  given lifts: AnyRef 
    def (rs: ResultSet) row: Row = rs
    def (rs: ResultSet) header: Header = rs.getMetaData

  given (given outputType: OutputType): Convertible[Header, Json] = meta =>
    val columnJson: Int => Json = 
      outputType match
        case OutputType.ArrayWithHeader(true) =>
          (i: Int) => Json.obj(
            "label" -> meta.getColumnLabel(i).as[Json],
            "type" -> meta.getColumnTypeName(i).as[Json]
          )
        case _ =>  meta.getColumnLabel(_: Int).as[Json]

    Json.Arr((1 to meta.getColumnCount).map(columnJson).toVector)

  given (given outputType: OutputType): Convertible[Row, Json] = rs =>
    val meta = rs.getMetaData
    outputType match
      case OutputType.BareArray | OutputType.ArrayWithHeader(_) => Json.Arr((1 to meta.getColumnCount).toVector.map(rs.col(_, meta)))
      case OutputType.Object => 
        Json.Obj((1 to meta.getColumnCount).map { i => 
          meta.getColumnLabel(i) -> rs.col(i, meta)
        }.toMap)

  given ops: AnyRef
    def (rs: Row) col(index: Int, meta: ResultSetMetaData): Json =
      meta.getColumnType(index) match
        case Types.ARRAY =>
          val array = rs.getArray(index)
          if rs.wasNull then Json.nil
          else 
            try
              val arrayRs = array.getResultSet
              given ResultSetMetaData = arrayRs.getMetaData
              given OutputType = OutputType.BareArray
              try arrayRs.as[Row].as[Json]
              finally arrayRs.close()
            finally array.free()
        case Types.BIGINT => 
          val r = rs.getLong(index)
          if (rs.wasNull) Json.nil else r.as[Json]
        case Types.BINARY | Types.VARBINARY | Types.LONGVARBINARY => 
          val r = rs.getBytes(index)
          if (rs.wasNull) Json.nil else r.as[Json]
        case Types.BIT => 
          val r = rs.getBoolean(index)
          if (rs.wasNull) Json.nil else r.as[Json]
        case Types.BOOLEAN => 
          val r = rs.getBoolean(index)
          if (rs.wasNull) Json.nil else r.as[Json]
        case Types.CHAR | Types.VARCHAR | Types.LONGNVARCHAR => 
          val r = rs.getString(index)
          if (rs.wasNull) Json.nil else r.as[Json]
        case Types.DATE => 
          val r = rs.getDate(index)
          if (rs.wasNull) Json.nil else r.toString.as[Json]
        case Types.DECIMAL | Types.NUMERIC => 
          val r = rs.getBigDecimal(index)
          if (rs.wasNull) Json.nil else BigDecimal(r).as[Json]
        case Types.DOUBLE | Types.FLOAT => 
          val r = rs.getDouble(index)
          if (rs.wasNull) Json.nil else r.as[Json]
        case Types.INTEGER | Types.SMALLINT | Types.TINYINT => 
          val r = rs.getInt(index)
          if (rs.wasNull) Json.nil else r.as[Json]
        case Types.JAVA_OBJECT | Types.OTHER | Types.STRUCT => 
          val r = rs.getObject(index)
          if (rs.wasNull) Json.nil else r.toString.as[Json]
        case Types.NULL => Json.nil
        case Types.REAL => 
          val r = rs.getFloat(index)
          if (rs.wasNull) Json.nil else r.as[Json]
        case Types.ROWID => 
          val r = rs.getRowId(index)
          if (rs.wasNull) Json.nil else r.toString.as[Json]
        case Types.SQLXML =>
          val xml = rs.getSQLXML(index)
          if (rs.wasNull) Json.nil
          else try xml.toString.as[Json] finally xml.free()
        case Types.TIME | Types.TIME_WITH_TIMEZONE => 
          val r = rs.getTime(index)
          if (rs.wasNull) Json.nil else r.toString.as[Json]
        case Types.TIMESTAMP | Types.TIMESTAMP_WITH_TIMEZONE => 
          val r = rs.getTimestamp(index)
          if (rs.wasNull) Json.nil else r.toString.as[Json]
          
        case Types.BLOB => throw new NotImplementedError(s"BLOB not supported (column ${meta.getColumnLabel(index)})")
        case Types.CLOB => throw new NotImplementedError(s"CLOB not supported (column ${meta.getColumnLabel(index)})")
        case Types.DATALINK => throw new NotImplementedError(s"DATALINK not supported (column ${meta.getColumnLabel(index)})")
        case Types.DISTINCT => throw new NotImplementedError(s"DISTINCT not supported (column ${meta.getColumnLabel(index)})")
        case Types.NCHAR => throw new NotImplementedError(s"NCHAR not supported (column ${meta.getColumnLabel(index)})")
        case Types.NCLOB => throw new NotImplementedError(s"NCLOB not supported (column ${meta.getColumnLabel(index)})")
        case Types.NVARCHAR => throw new NotImplementedError(s"NVARCHAR not supported (column ${meta.getColumnLabel(index)})")
        case Types.REF => throw new NotImplementedError(s"REF not supported (column ${meta.getColumnLabel(index)})")
        case Types.REF_CURSOR => throw new NotImplementedError(s"REF_CURSOR not supported (column ${meta.getColumnLabel(index)})")