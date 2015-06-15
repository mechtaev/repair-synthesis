package edu.nus.mrepair.vcc.translation

import edu.nus.mrepair.vcc.translation.TranslationCommon._
import edu.nus.mrepair.SuspiciousLocations
import edu.nus.mrepair.SynthesisConfig
import edu.nus.mrepair.vcc.VCCUtils._
import org.smtlib.IExpr
import org.smtlib.IExpr._
import edu.nus.mrepair.vcc.translation.TranslationCommon.TF
import scala.collection.JavaConverters._
import org.smtlib.impl.SMTExpr
import org.smtlib.impl.SMTExpr.StringLiteral
import edu.nus.vctrans.ast.DeclareFunAst
import edu.nus.vctrans.ast.{DefineFunAst, CFunctionAst}
import org.smtlib.command.{C_assert, C_define_fun}
import edu.nus.mrepair.synthesis.ProgramFormula.ProgramVariable
import edu.nus.mrepair.StatementLevelRepair.RepairableBindings
import edu.nus.mrepair.synthesis.ProgramFormula

object RepairableSubstitutor {

  def CONDITION: IStringLiteral      = new StringLiteral("CONDITION", false)
  def RHS: IStringLiteral            = new StringLiteral("RHS", false)
  def LOOP_CONDITION: IStringLiteral = new StringLiteral("LOOP-CONDITION", false)
  def CALL: IStringLiteral           = new StringLiteral("CALL", false)
  def INDEX: IStringLiteral          = new StringLiteral("INDEX", false)

  var collected: Map[(Int, Int), String] = null
  var subsVarsCount: Int = 0

  // here we need main function as an argument because it is represented differently now (as an assertion)
  def substitute(tf: TF, declsIndex: Map[String, DeclareFunAst], suspicious: SuspiciousLocations, mainFunction: String, synthesisConfig: SynthesisConfig): (TF, TFRepairableExpressions) = {
    collected = Map[(Int, Int), String]() // (stmtId, exeId) -> var
    subsVarsCount = 0

    val isInteresting = suspicious match {
      case SuspiciousLocations(functions, _) => (f: String) => functions.contains(f)
    }

    val (newProcedures, funcBindings) = tf.procedures.map({
      case Procedure(name, assumption, instances, order) =>
        if (isInteresting(name)) {
          val body = assumption.getExpr
          val (newBody, rawRep) =
            if (name == mainFunction) substituteExprAux(body, declsIndex, synthesisConfig)
            else (removeAnnotations(body), Nil)
          val newAssumption = new CFunctionAst(name)
          newAssumption.setExpr(newBody)
          val (newInstances, rawDReps) = instances.map({
            case d =>
              val (newDBody, rawDRep) = substituteExprAux(d.getBodyExp, declsIndex, synthesisConfig)
              val newDefine =
                new DefineFunAst(new C_define_fun(d.jSMTLIBAst.symbol(), d.jSMTLIBAst.parameters(), d.jSMTLIBAst.resultSort(), newDBody))
              (newDefine, rawDRep)
          }).unzip
          val raw = (rawRep :: rawDReps).flatten.flatten //FIXME here we should not do second flatten
          val ripe = raw.map({
            case (varName, smtlibExpr, bound, stmtId, exeId) =>
              val astTypeOf = (n: String) => lookupGlobalDeclarations(declsIndex, n)
              translateIfRepairable(smtlibExpr, astTypeOf) match {
                case Some(expr) =>
                  (ProgramVariable(varName, ProgramFormula.Types.typeOfExpr(expr)), expr, bound, stmtId, exeId)
                case _ =>
                  ??? //TODO if not repairable, we need to add it without RC
            }
          })
          (Procedure(name, newAssumption, newInstances, order), (name, ripe) :: Nil)
        } else {
          val body = assumption.getExpr
          val newBody = removeAnnotations(body)
          val newAssumption = new CFunctionAst(name)
          newAssumption.setExpr(newBody)
          val newInstances = instances.map({
            case d =>
              val newDBody = removeAnnotations(d.getBodyExp)
            new DefineFunAst(new C_define_fun(d.jSMTLIBAst.symbol(), d.jSMTLIBAst.parameters(), d.jSMTLIBAst.resultSort(), newDBody))
          })
          (Procedure(name, newAssumption, newInstances, order), Nil)
        }
    }).unzip

    val (newGlobalDecls, globalDeclsBindings) = 
      tf.globalDecls.map((assertion: C_assert) => { 
        val (newExpr, bindings) = RepairableSubstitutor.substituteExpr(tf, declsIndex, suspicious.globalVariables, assertion.expr(), synthesisConfig)
        (new C_assert(newExpr), bindings)
      }).unzip

    val repairable = TFRepairableExpressions(globalDeclsBindings, funcBindings.flatten.toMap)

    (TF(tf.sorts, tf.declarations, tf.asserts, newProcedures, newGlobalDecls), repairable)
  }

  private def removeAnnotations(exp: IExpr): IExpr = {
    SMTLIBExpr.mapT(exp, {
      case a: IAttributedExpr => a.expr()
      case e => e
    })
  }
  
  
  private def substituteExpr(tf: TF, declsIndex: Map[String, DeclareFunAst], needSubs: Boolean, expr: IExpr, synthesisConfig: SynthesisConfig): (IExpr, RepairableBindings) = {
    if (needSubs) {
      val (subsExpr, raw) = substituteExprAux(expr, declsIndex, synthesisConfig)
      val ripe = raw.flatten.map({
        case (varName, smtlibExpr, bound, stmtId, exeId) =>
          val astTypeOf = (n: String) => lookupGlobalDeclarations(declsIndex, n)
          translateIfRepairable(smtlibExpr, astTypeOf) match {
            case Some(expr) =>
              (ProgramVariable(varName, ProgramFormula.Types.typeOfExpr(expr)), expr, bound, stmtId, exeId)
            case _ =>
              ??? //TODO if not repairable, we need to add it without RC
          }
      })
      (subsExpr, ripe)
    } else {
      (removeAnnotations(expr), Nil)
    }
  }

  private def substituteExprAux(expr: IExpr, declsIndex: Map[String, DeclareFunAst], synthesisConfig: SynthesisConfig): (IExpr, List[List[(String, IExpr, Option[Int], Int, Int)]]) = {

    SMTLIBExpr.fold[(IExpr, List[List[(String, IExpr, Option[Int], Int, Int)]])](expr, {
      case exp: IAttributedExpr => {
        case (inner, rep) :: Nil =>
          val attrs = exp.attributes().asScala.toList
          attrs.find({ case a => a.keyword().equals(CmpId) }) match {
            case None => (inner, rep)
            case Some(idAttr) =>
              val id = idAttr.attrValue().asInstanceOf[SMTExpr.Numeral].intValue()
              val exe = attrs.find({ case a => a.keyword().equals(CmpExe)}) match {
                case None => 0 //TODO default value vs no value?
                case Some(exeAttr) => exeAttr.attrValue().asInstanceOf[SMTExpr.Numeral].intValue()
              }
              attrs.find({ case a => a.keyword().equals(CmpKind) }) match {
                case None => (inner, rep)
                case Some(kindAttr) =>
                  val bound = attrs.find({ 
                    case a => a.keyword().equals(CmpBound)
                  }).map({ 
                    case attr => attr.attrValue().asInstanceOf[SMTExpr.Numeral].intValue()
                  })
                  val av = kindAttr.attrValue()
                  if (av.equals(CONDITION) || av.equals(RHS) || av.equals(CALL)) {
                    if(collected.contains((id, exe))) {
                      (new SMTExpr.Symbol(collected((id, exe))), Nil)
                    } else {
                      val astTypeOf = (n: String) => lookupGlobalDeclarations(declsIndex, n)
                      if ((! synthesisConfig.spaceReduction) || 
                          isInterestingExpression(inner, astTypeOf, synthesisConfig)) {
                        val newName = "subs" + subsVarsCount.toString
                        collected = collected + (((id, exe), newName))
                        subsVarsCount += 1
                        (new SMTExpr.Symbol(newName), ((newName, inner, bound, id, exe) :: Nil) :: Nil)
                      } else {
                        (inner, rep)
                      }
                    }
                  } else (inner, rep)
              }
          }
      }
      case exp: IFcnExpr => {
        case res =>
          val (args, rep) = res.unzip
          (new SMTExpr.FcnExpr(exp.head(), args.asJava), rep.flatten)
      }
      case exp: IForall => ???
      case exp: ILet => {
        case res =>
          val (args, rep) = res.unzip
          val params = exp.bindings().asScala.toList.map((b: IBinding) => b.parameter())
          val (bindingExprs, body :: Nil) = args.splitAt(args.length - 1)
          val newBindings =
            params.zip(bindingExprs).map({
              case (p, b) => new SMTExpr.Binding(p, b).asInstanceOf[IBinding]
            })
          (new SMTExpr.Let(newBindings.asJava, body), rep.flatten)
      }
      case exp: ISymbol => {
        case Nil => (exp, Nil)
      }
      case exp: INumeral => {
        case Nil => (exp, Nil)
      }
      case _ => ???
    })

  }

}
