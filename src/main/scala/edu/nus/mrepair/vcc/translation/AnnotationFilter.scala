package edu.nus.mrepair.vcc.translation

import edu.nus.mrepair.vcc.translation.TranslationCommon._
import org.smtlib.IExpr
import org.smtlib.command.C_define_fun
import edu.nus.vctrans.ast.{CFunctionAst, DefineFunAst}
import scala.collection.JavaConverters._
import org.smtlib.IExpr._
import edu.nus.mrepair.vcc.translation.TranslationCommon.Procedure
import edu.nus.mrepair.vcc.translation.TranslationCommon.TF
import org.smtlib.command.{C_assert, C_define_fun}

object AnnotationFilter {

  /**
   * Remove wrong annotations
   *
   * Currently, if encounter nested annotations, remove the outer one
   */
  def filter(tf: TF): TF = {
    val newProcedures = tf.procedures.map({
      case Procedure(name, assumption, instances, order) =>
        val newBody = removeNested(assumption.getExpr)
        val newAssumption = new CFunctionAst(name)
        newAssumption.setExpr(newBody)
        val newInstances = instances.map({
          case d =>
            val newDBody = removeNested(d.getBodyExp)
            val newDefine =
              new DefineFunAst(new C_define_fun(d.jSMTLIBAst.symbol(), d.jSMTLIBAst.parameters(), d.jSMTLIBAst.resultSort(), newDBody))
            newDefine
        })
        Procedure(name, newAssumption, newInstances, order)
    })

    val newGlobalDecls = tf.globalDecls.map({
      case gd => new C_assert(removeNested(gd.expr()))
    })

    TF(tf.sorts, tf.declarations, tf.asserts, newProcedures, newGlobalDecls)
  }

  def removeNested(expr: IExpr): IExpr = {
    SMTLIBExpr.mapT(expr, {
      case a: IAttributedExpr =>
        if (hasValidNested(a.expr())) a.expr()
        else a
      case e => e
    })
  }

  def hasValidNested(expr: IExpr): Boolean = {
    SMTLIBExpr.fold[Boolean](expr, {
      case exp: IAttributedExpr => {
        case found :: Nil =>
          found || {
            val attrs = exp.attributes().asScala.toList
            attrs.find({ case a => a.keyword().equals(CmpId)}) match {
              case None => false
              case Some(idAttr) =>
                attrs.find({ case a => a.keyword().equals(CmpKind)}) match {
                  case None => false
                  case Some(kindAttr) => true
                }
            }
          }
      }
      case exp: IFcnExpr => {
        case children => children.foldLeft(false)(_ || _)
      }
      case exp: ILet => {
        case children => children.foldLeft(false)(_ || _)
      }
      case exp: ISymbol => {
        case Nil => false
      }
      case exp: INumeral => {
        case Nil => false
      }
      case _ => ???
    })

  }

}
