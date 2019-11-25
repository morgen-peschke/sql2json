package sql2json
package types
package json 

import scala.math.Numeric
import cat.Cofunctor
import cat.Cofunctor.given

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

  given[A]: SerializeOps[A]

  given Cofunctor[Serialize]
    def comap [A,B] (fa: Serialize[A], f: B => A): Serialize[B] = b => fa.toJson(f(b))

  given Serialize[Boolean] = Json.Bool(_)
  given Serialize[String] = Json.Text(_)

  given[A](given Serialize[A]): Serialize[Vector[A]] = va => Json.Array(va.map(_.toJson))

  private val byteEncoder = java.util.Base64.getEncoder
  given Serialize[Array[Byte]] = ba => Json.Text(byteEncoder.encodeToString(ba))

  given bdSerialize: Serialize[BigDecimal] = Json.Number(_)

  given Serialize[java.math.BigDecimal] = bd => Json.Number(BigDecimal(bd))

  given Serialize[BigInt] = bdSerialize.comap(BigDecimal(_))

  given Serialize[Int] = bdSerialize.comap(BigDecimal(_))

  given Serialize[Long] = bdSerialize.comap(BigDecimal(_))

  given Serialize[Float] = bdSerialize.comap(f => BigDecimal(f.toDouble))

  given Serialize[Double] = bdSerialize.comap(BigDecimal(_))
