package sql2json
package jdbc

import java.sql.{ResultSet,ResultSetMetaData,Types}
import types.json.Json
import types.json.Serialize.syntax
import Row.asRow

sealed abstract class OutputType 
object OutputType
  case object Array extends OutputType
  case object Object extends OutputType

opaque type Row = ResultSet
object Row
  def (rs: ResultSet) asRow: Row = rs

  def (rs: Row) resultSetAsJson(outputType: OutputType)(given meta: ResultSetMetaData): Json = 
    outputType match
      case OutputType.Array => Json.Array((1 to meta.getColumnCount).toVector.map(rs.col(_)))
      case OutputType.Object => 
        Json.Object((1 to meta.getColumnCount).map { i => 
          meta.getColumnLabel(i) -> rs.col(i)
        }.toMap)

  def (rs: Row) col(index: Int)(given meta: ResultSetMetaData): Json =
    meta.getColumnType(index) match
      case Types.ARRAY =>
        val array = rs.getArray(index)
        if rs.wasNull then Json.nil
        else 
          try
            val arrayRs = array.getResultSet
            try arrayRs.asRow.resultSetAsJson(OutputType.Array)(given arrayRs.getMetaData)
            finally arrayRs.close()
          finally array.free()
      case Types.BIGINT => 
        val r = rs.getLong(index)
        if (rs.wasNull) Json.nil else r.toJson
      case Types.BINARY | Types.VARBINARY | Types.LONGVARBINARY => 
        val r = rs.getBytes(index)
        if (rs.wasNull) Json.nil else r.toJson
      case Types.BIT => 
        val r = rs.getBoolean(index)
        if (rs.wasNull) Json.nil else r.toJson
      case Types.BOOLEAN => 
        val r = rs.getBoolean(index)
        if (rs.wasNull) Json.nil else r.toJson
      case Types.CHAR | Types.VARCHAR | Types.LONGNVARCHAR => 
        val r = rs.getString(index)
        if (rs.wasNull) Json.nil else r.toJson
      case Types.DATE => 
        val r = rs.getDate(index)
        if (rs.wasNull) Json.nil else r.toString.toJson
      case Types.DECIMAL | Types.NUMERIC => 
        val r = rs.getBigDecimal(index)
        if (rs.wasNull) Json.nil else r.toJson
      case Types.DOUBLE | Types.FLOAT => 
        val r = rs.getDouble(index)
        if (rs.wasNull) Json.nil else r.toJson
      case Types.INTEGER | Types.SMALLINT | Types.TINYINT => 
        val r = rs.getInt(index)
        if (rs.wasNull) Json.nil else r.toJson
      case Types.JAVA_OBJECT | Types.OTHER | Types.STRUCT => 
        val r = rs.getObject(index)
        if (rs.wasNull) Json.nil else r.toString.toJson
      case Types.NULL => Json.nil
      case Types.REAL => 
        val r = rs.getFloat(index)
        if (rs.wasNull) Json.nil else r.toJson
      case Types.ROWID => 
        val r = rs.getRowId(index)
        if (rs.wasNull) Json.nil else r.toString.toJson
      case Types.SQLXML =>
        val xml = rs.getSQLXML(index)
        if (rs.wasNull) Json.nil
        else try xml.toString.toJson finally xml.free()
      case Types.TIME | Types.TIME_WITH_TIMEZONE => 
        val r = rs.getTime(index)
        if (rs.wasNull) Json.nil else r.toString.toJson
      case Types.TIMESTAMP | Types.TIMESTAMP_WITH_TIMEZONE => 
        val r = rs.getTimestamp(index)
        if (rs.wasNull) Json.nil else r.toString.toJson
        
      case Types.BLOB => throw new NotImplementedError(s"BLOB not supported (column ${meta.getColumnLabel(index)})")
      case Types.CLOB => throw new NotImplementedError(s"CLOB not supported (column ${meta.getColumnLabel(index)})")
      case Types.DATALINK => throw new NotImplementedError(s"DATALINK not supported (column ${meta.getColumnLabel(index)})")
      case Types.DISTINCT => throw new NotImplementedError(s"DISTINCT not supported (column ${meta.getColumnLabel(index)})")
      case Types.NCHAR => throw new NotImplementedError(s"NCHAR not supported (column ${meta.getColumnLabel(index)})")
      case Types.NCLOB => throw new NotImplementedError(s"NCLOB not supported (column ${meta.getColumnLabel(index)})")
      case Types.NVARCHAR => throw new NotImplementedError(s"NVARCHAR not supported (column ${meta.getColumnLabel(index)})")
      case Types.REF => throw new NotImplementedError(s"REF not supported (column ${meta.getColumnLabel(index)})")
      case Types.REF_CURSOR => throw new NotImplementedError(s"REF_CURSOR not supported (column ${meta.getColumnLabel(index)})")