package edu.nus.mrepair.klee

import org.smtlib.IExpr
import edu.nus.mrepair._
import edu.nus.mrepair.synthesis.ProgramFormula
import edu.nus.mrepair.synthesis.Formula._
import edu.nus.mrepair.StatementLevelRepair._
import edu.nus.mrepair.decision._
import edu.nus.mrepair.AngelicFix._
import edu.nus.mrepair.synthesis.ComponentDecoder
import edu.nus.mrepair.synthesis.ComponentFormula.Component
import edu.nus.mrepair.vcc.translation.TranslationCommon._
import edu.nus.mrepair.StatementLevelRepair.RepairableBindings
import edu.nus.mrepair.vcc.VCCUtils.TFRepairableExpressions
import edu.nus.mrepair.vcc.VCCUtils
import ProgramFormula._
import ProgramFormula.ConstantConversion._
import edu.nus.mrepair.Utils._
import edu.nus.mrepair.Utils.SimpleLogger._

object RCGenerator {

  def debugMode(): Boolean = {
    sys.env.get("AF_DEBUG") match {
      case None => false
      case _ => true
    }
  }

  //TODO this is a hack that is implemented inconsistently
  def bindingVar(stmtId: Int, instId: Int): String = "bindingvar" + stmtId.toString + "#" + instId.toString

  //TODO if a variable is used as both integer and boolean, I need wrap it by component (x != 0)
  def correctTypes(afRaw: AngelicForest, suspicious: List[(Int, IExpr)]): (AngelicForest, RepairableBindings) = {
    var af = afRaw

    // check what is the angelic value type for particular statement
    def expectedTopTypeIsBool(stmtId: Int): Boolean = {
      val isBooleanList = af.map({
        case (_, ap) =>
          ap.map({
            case vals =>
              vals.filter({
                case AngelicValue(_, _, id, _) => id == stmtId
              }).map({
                case AngelicValue(_, IntVal(_, _), id, _) => false
                case AngelicValue(_, BoolVal(_, _), id, _) => true
              })
          }).flatten
      }).flatten

      val disj = isBooleanList.foldLeft(false)(_ || _)
      val conj = isBooleanList.foldLeft(true)(_ && _)

      if (disj && !conj) {
        println("[synthesis] WARNING: inconsistent angelic values types for stmt " + stmtId)
      }

      disj
    }

    def fixAF(constraints: Set[String], topTypeIsBool: Boolean, stmtId: Int): Unit = {
      def fixContext(vals: List[VariableValue]): List[VariableValue] = {
        vals.map({
          case IntVal(name, value) =>
            if (constraints.contains(name)) BoolVal(name, value != 0) else IntVal(name, value)
          case other => other
        })
      }

      af = af.map({
        case (something, ap) =>
          (something, ap.map({
            case vals =>
              vals.map({
                case AngelicValue(ctx, IntVal(name, value), id, inst) if id == stmtId =>
                  AngelicValue(fixContext(ctx), if (topTypeIsBool) BoolVal(name, value != 0) else IntVal(name, value), id, inst)
                case AngelicValue(ctx, BoolVal(name, value), id, inst) if id == stmtId =>
                  AngelicValue(fixContext(ctx), BoolVal(name, value), id, inst)
                case other => other
              })
          }))
      })
    }

    val suspiciousWithTypeinfo = suspicious.map({
      case (stmtId, expr) =>
        val expectedTypeIsBool = expectedTopTypeIsBool(stmtId)
        val (typeConstraints, topIsBoolean) = VCCUtils.getTypeConstraints(expr, expectedTypeIsBool)
        val fixedExpr = if (expectedTypeIsBool && !topIsBoolean) VCCUtils.castIntToBool(expr) else expr
        fixAF(typeConstraints, topIsBoolean, stmtId)
        def typeOf(s: String): Type = {
          if (typeConstraints.contains(s)) BooleanType()
          else IntegerType()
        }
        (stmtId, fixedExpr, typeOf(_), topIsBoolean || expectedTypeIsBool)
    })

    val repairableBindings =
      suspiciousWithTypeinfo.map({
        case (stmtId, expr, typeOf, topIsBool) =>
          //TODO support instances
          val Some(pfe) = VCCUtils.translateIfRepairable(expr, { case n => Some(typeOf(n)) })
          (ProgramVariable(bindingVar(stmtId, 0), if (topIsBool) BooleanType() else IntegerType()), pfe, None, stmtId, 1)
      })

    (af, repairableBindings)
  }


  def generate(angelicForestRaw: AngelicForest,
               suspicious: List[(Int, IExpr)],
               repairConfig: SynthesisConfig): (RepairCondition, List[(String, ProgramFormulaExpression)], List[Component]) = {

    val (angelicForest, repairableBindings) = correctTypes(angelicForestRaw, suspicious)

    if (debugMode()) {
      println("[synthesis] Angelic forest with inferred types:")
      angelicForest.foreach({ case (t, aps) => println("[synthesis] test " + t + ": " + aps) })
    }

    val (repairableObjects, extractedComponents) = 
      extractRepairableObjects(repairableBindings, repairConfig.synthesisConfig, repairConfig.componentLevel)

    //FIXME: when I support instances, it should be done using repairable objects
    val oldExprs = repairableBindings.map({
      case (ProgramVariable(name, _), pfe, _, _, _) => (name, pfe)
    })

    //should select components somehow (existing + additional + shared)
    val sharedComponents: List[Component] = Nil

    val (softStructureExpr, hardStructureExpr) = generateStructureConstraints(repairableObjects, repairConfig.synthesisConfig)

    val (softStructure, hardStructure) = (softStructureExpr.map(FormulaAST), hardStructureExpr.map(FormulaAST))

    //TODO if there are multiple suspicious expressions, context for them can have same variables that can have different values
    val semanticsConstraints = angelicForest.values.zipWithIndex.map({
      case (ap, testId) =>
        val angelicPaths = ap.flatten
        val formula = angelicPaths match {
          case Nil => BooleanValue[ProgramVariable](true)
          case _ =>
            val start: ProgramFormulaExpression = BooleanValue[ProgramVariable](false)
            ap.flatten.foldLeft(start)({
              case (acc, AngelicValue(context, value, _, _)) =>
                val angelic = value match {
                  case BoolVal(id, v) => (bvar(id) <=> v)
                  case IntVal(id, v) => (ivar(id) === v)
                }
                val clause = context.foldLeft(angelic)({
                  case (e, IntVal(n, v)) => (ivar(n) === v) & e
                  case (e, BoolVal(n, v)) => (bvar(n) <=> v) & e
                  })
                (clause | acc)
            })
        }

        val synthesisPart = semanticsConstraintsForTestCase(repairableObjects, sharedComponents, testId, repairConfig.synthesisConfig).map(FormulaAST)

        FormulaAST(testConstraintsForTestCase(formula, testId)) :: synthesisPart
    }).flatten


    val components = extractedComponents ++ sharedComponents

    (RepairCondition(hardStructure ++ semanticsConstraints, softStructure), oldExprs, components)
  }


  def solve(rc: RepairCondition,
            components: List[Component],
            oldExpressions: List[(String, ProgramFormulaExpression)],
            repairConfig: SynthesisConfig): (Either[List[(Int, Int, ProgramFormulaExpression, ProgramFormulaExpression)], Boolean], SolverStat) = {

    val RepairCondition(hardClauses, softClauses) = rc

    val solverResult =
      MaxSMTPlay.solve(hardClauses, softClauses, components, repairConfig.simplification, repairConfig.solverBound, repairConfig.solverTimeout)

    solverResult match {
      case Right(isTimeout) =>
        (Right(isTimeout), MaxSMTPlay.lastStat)
      case Left((numRemovedConstr, model)) =>
        val newAssignments = ComponentDecoder.decode(model)
        val old = oldExpressions.toMap
        val changes = newAssignments.map({
          case (v, expr) =>
            val s :: i :: Nil = v.name.drop("bindingvar".length).split("#").map(_.toInt).toList
            (s, i, old(v.name), expr)
        })
        
        (Left(changes), MaxSMTPlay.lastStat)
    }

  }


}
