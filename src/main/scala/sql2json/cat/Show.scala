package sql2json
package cat
import scala.quoted._
import scala.quoted.matching._

trait Show[-A]
  def show(a: A): String

object Show
  trait ShowOps[A]
    def (a: A) show(given S: Show[A]): String = S.show(a)

  given[A]: ShowOps[A]

  given Show[Nothing] = _ => ??? // Should never be used, but needed for stuff like Nil.show to compile
  given Show[String] = str => s""""$str""""
  given Show[Int] = _.toString
  given Show[Long] = l => s"${l}L"
  given Show[Boolean] = _.toString
  given [L: Show, R: Show]: Show[Either[L,R]] = 
    _ match
      case Left(l) => s"Left(${l.show})"
      case Right(r) => s"Right(${r.show})"
  given [L: Show]: Show[Left[L, ?]] = left => s"Left(${left.value.show})"
  given [R: Show]: Show[Right[?, R]] = right => s"Right(${right.value.show})"

  given [A: Show]: Show[List[A]] = _.map(_.show).mkString("[", ",", "]")

  trait ShowInterpolator
    inline def (sc: StringContext) show(args: =>Any*): String = ${ interpolatorImpl('sc, 'args) }

  given ShowInterpolator

  private def interpolatorImpl(sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])(given qctx: QuoteContext): Expr[String] =
    def fail[A](msg: String, expr: Expr[?]): Expr[A] =
      qctx.error(msg, expr)
      '{???}

    argsExpr match {
      case ExprSeq(argExprs) =>
        val newArgsExpr = Expr.ofSeq(argExprs.map {
          case '{ $arg: $tp } =>
            val showTp = '[Show[$tp]]
            searchImplicitExpr(given showTp, summon[QuoteContext]) match 
              case Some(showExpr) => '{ $showExpr.show($arg) }
              case None => fail(s"could not find implicit for ${showTp.show}", arg)
          case arg => fail(s"unexpected format: ${arg.show}", arg)
        })
        '{ $sc.s($newArgsExpr: _*) }
      case _ => fail(s"Args must be explicit", argsExpr)
    }