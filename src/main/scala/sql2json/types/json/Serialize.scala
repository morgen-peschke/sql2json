package sql2json
package types
package json 

import scala.math.Numeric

/**
 * Very minimal serialization of arbitrary types to [[Json]].
 *
 * It's pretty bare-bones because we don't really need to convert anything
 * more complicated than [[scala.collection.immutable.Vector]].
 */
trait Serialize[A]
    def toJson(a: A): Json

object Serialize

  trait SerializeOps[A]
    def (a: A) toJson (given S: Serialize[A]): Json = S.toJson(a)

  given syntax[A]: SerializeOps[A]

  given Serialize[Boolean]
    def toJson(a: Boolean): Json = Json.Bool(a)
 
  given Serialize[String]
    def toJson(a: String): Json = Json.Text(a)

  given vectorSerializer[A](given Serialize[A]): Serialize[Vector[A]]
    def toJson(va: Vector[A]): Json = Json.Array(va.map(_.toJson))

  private val byteEncoder = java.util.Base64.getEncoder
  given Serialize[Array[Byte]]
    def toJson(ba: Array[Byte]): Json = Json.Text(byteEncoder.encodeToString(ba))

  given Serialize[BigDecimal]
    def toJson(a: BigDecimal): Json = Json.Number(a)

  given serializeJavaBigDecimal: Serialize[java.math.BigDecimal]
    def toJson(a: java.math.BigDecimal): Json = Json.Number(BigDecimal(a))

  given Serialize[BigInt]
    def toJson(a: BigInt): Json = Json.Number(BigDecimal(a))

  given Serialize[Int]
    def toJson(a: Int): Json = Json.Number(BigDecimal(a))

  given Serialize[Long]
    def toJson(a: Long): Json = Json.Number(BigDecimal(a))

  given Serialize[Float]
    def toJson(a: Float): Json = Json.Number(BigDecimal(a.toDouble))

  given Serialize[Double]
    def toJson(a: Double): Json = Json.Number(BigDecimal(a))