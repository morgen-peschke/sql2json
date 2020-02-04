package sql2json
package cat
package macros

import scala.quoted._
import scala.quoted.matching._

trait ShowMacros
  inline def (sc: StringContext) show(args: =>Any*): String = ${ ShowMacros.showMacroImpl('sc, 'args) }

object ShowMacros
  inline def (sc: StringContext) show(args: =>Any*): String = ${ ShowMacros.showMacroImpl('sc, 'args) }

  def showMacroImpl(sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])(given qctx: QuoteContext): Expr[String] =
    def fail[A](msg: String, expr: Expr[?]): Expr[A] =
      qctx.error(msg, expr)
      '{???}

    argsExpr match {
      case ExprSeq(argExprs) =>
        val newArgsExpr = Expr.ofSeq(argExprs.map {
          case '{ $arg: $tp } =>
            summonExpr[Show[$tp]] match 
              case Some(showExpr) => '{ $showExpr.show($arg) }
              case None => fail(s"could not find implicit for Show[${tp.show}]", arg)
          case arg => fail(s"unexpected format: ${arg.show}", arg)
        })
        '{ $sc.s($newArgsExpr: _*) }

      case _ => fail(s"Args must be explicit", argsExpr)
    }