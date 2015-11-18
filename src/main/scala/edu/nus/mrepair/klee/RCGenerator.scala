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

  def getAllInstances(af: AngelicForest, stmtId: String): List[Int] = {
    af.map({
      case (_, aps) =>
        aps.map({
          case ap =>
            ap.map({
              case AngelicValue(_, _, s, i) if s == stmtId => i :: Nil
              case _ => Nil
            }).flatten
        }).flatten
    }).flatten.toList.distinct
  }

  // TODO instead of this I need to give proper names to all instances
  def renameInst(name: String, stmtId: Int, instId: Int): String = name + "#" + stmtId.toString + "#" + instId.toString

  //TODO if a variable is used as both integer and boolean, I need wrap it by component (x != 0)
  def correctTypes(afRaw: AngelicForest, suspicious: List[(Int, IExpr)], suspiciousIds: List[String]): (AngelicForest, RepairableBindings) = {
    var af = afRaw

    // check what is the angelic value type for particular statement
    def expectedTopTypeIsBool(stmtId: String): Boolean = {
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
        System.err.println("WARNING: inconsistent angelic values types for stmt " + stmtId)
      }

      disj
    }

    def fixAF(constraints: Set[String], topTypeIsBool: Boolean, stmtId: String): Unit = {
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
        val expectedTypeIsBool = expectedTopTypeIsBool(suspiciousIds(stmtId))
        val (typeConstraints, topIsBoolean) = VCCUtils.getTypeConstraints(expr, expectedTypeIsBool)
        val fixedExpr = if (expectedTypeIsBool && !topIsBoolean) VCCUtils.castIntToBool(expr) else expr
        fixAF(typeConstraints, topIsBoolean, suspiciousIds(stmtId))
        def typeOf(s: String): Type = {
          if (typeConstraints.contains(s)) BooleanType()
          else IntegerType()
        }
        (stmtId, fixedExpr, typeOf(_), topIsBoolean || expectedTypeIsBool)
    })

    val repairableBindings =
      suspiciousWithTypeinfo.map({
        case (stmtId, expr, typeOf, topIsBool) =>
          val instIds = getAllInstances(af, suspiciousIds(stmtId))
          instIds.map({
            case instId =>
              val Some(pfe) = VCCUtils.translateIfRepairable(expr, { case n => Some(typeOf(n)) })
              val pfeRenamed = ProgramFormulaUtils.substitute(pfe, { case ProgramVariable(n, t) => Variable(ProgramVariable(renameInst(n, stmtId, instId), t)) } , { case u => Variable(u) })
              (ProgramVariable(renameInst("angelic", stmtId, instId), if (topIsBool) BooleanType() else IntegerType()), pfeRenamed, None, stmtId, instId)
          })
      }).flatten

    (af, repairableBindings)
  }


  def generate(angelicForestRaw: AngelicForest,
               suspicious: List[(Int, IExpr)],
               suspiciousIds: List[String],
               repairConfig: SynthesisConfig): (RepairCondition, List[(String, ProgramFormulaExpression)], List[Component]) = {

    val (angelicForest, repairableBindings) = correctTypes(angelicForestRaw, suspicious, suspiciousIds)

    System.err.println("Angelic forest with inferred types:")
    angelicForest.foreach({ case (t, aps) => System.err.println("test " + t + ": " + aps) })

    val usedVariables = suspicious.map({
      case (stmtId, expr) =>
        (stmtId, VCCUtils.collectVarNames(expr))
    }).toMap

    val additionalVariables = angelicForest.map({
      case (_, aps) =>
        aps.map({
          case ap =>
            ap.map({
              case AngelicValue(ctx, _, stmtId, _) =>
                ctx.map({ case varval => (suspiciousIds.indexOf(stmtId), AngelicFix.getName(varval)) })
            }).flatten
        }).flatten
    }).flatten.groupBy(_._1).map({
      case (stmtId, vars) =>
        (stmtId, vars.toList.map(_._2).filter({ case n => !usedVariables(stmtId).contains(n)}).distinct)
    })

    VariableComponentSelector.angelicfixVariables = additionalVariables

    val (repairableObjects, extractedComponents) = 
      extractRepairableObjects(repairableBindings, repairConfig.synthesisConfig, repairConfig.componentLevel)

    //FIXME: when I support instances, it should be done using repairable objects
    val oldExprs = repairableBindings.map({
      case (ProgramVariable(name, _), pfe, _, _, _) => (name, pfe)
    })

    //should select components somehow (existing + additional + shared)
    val sharedComponents: List[Component] = Nil //getSharedComponents(repairConfig.componentLevel)

    val (softStructureExpr, hardStructureExpr) = generateStructureConstraints(repairableObjects, repairConfig.synthesisConfig)

    val (softStructure, hardStructure) = (softStructureExpr.map(FormulaAST), hardStructureExpr.map(FormulaAST))

    //TODO if there are multiple suspicious expressions, context for them can have same variables that can have different values
    val semanticsConstraints = angelicForest.values.zipWithIndex.map({
      case (aps, testId) =>
        val angelicPaths = aps.flatten
        val formula = angelicPaths match {
          case Nil => BooleanValue[ProgramVariable](true)
          case _ =>
            val constFalse: ProgramFormulaExpression = BooleanValue[ProgramVariable](false)
            val constTrue: ProgramFormulaExpression = BooleanValue[ProgramVariable](true)
            aps.map({
              case ap =>
                ap.foldLeft(constTrue)({
                  case (acc, AngelicValue(context, value, stmdId, instId)) =>
                    val angelic = value match {
                      case BoolVal(name, v) => (bvar(renameInst(name, suspiciousIds.indexOf(stmdId), instId)) <=> v)
                      case IntVal(name, v) => (ivar(renameInst(name, suspiciousIds.indexOf(stmdId), instId)) === v)
                    }
                    val clause = context.foldLeft(angelic)({
                      case (e, IntVal(name, v)) => (ivar(renameInst(name, suspiciousIds.indexOf(stmdId), instId)) === v) & e
                      case (e, BoolVal(name, v)) => (bvar(renameInst(name, suspiciousIds.indexOf(stmdId), instId)) <=> v) & e
                    })
                    (clause & acc)
            })}).foldLeft(constFalse)(_ | _)
        }

        val synthesisPart = semanticsConstraintsForTestCase(repairableObjects, sharedComponents, testId, repairConfig.synthesisConfig).map(FormulaAST)

        FormulaAST(testConstraintsForTestCase(formula, testId)) :: synthesisPart
    }).flatten


    val components = extractedComponents ++ sharedComponents
    if (Utils.enableLogging) prettyList(components).log("rc-components.log")

    (RepairCondition(hardStructure ++ semanticsConstraints, softStructure), oldExprs, components)
  }

  def beautifyResult(ugly: ProgramFormulaExpression): ProgramFormulaExpression = {
    ProgramFormulaUtils.substitute(ugly, { case ProgramVariable(n, t) => Variable(ProgramVariable(n.split("#").head, t)) } , { case u => Variable(u) })
  }


  def solve(rc: RepairCondition,
            components: List[Component],
            oldExpressions: List[(String, ProgramFormulaExpression)],
            repairConfig: SynthesisConfig): (Either[List[(Int, Int, ProgramFormulaExpression, ProgramFormulaExpression)], Boolean], SolverStat) = {

    val RepairCondition(hardClauses, softClauses) = rc

    val solverResult =
      MaxSMTPlay.solve(hardClauses, softClauses, components, repairConfig.simplification, repairConfig.reuseStructure, repairConfig.solverBound, repairConfig.solverTimeout)

    solverResult match {
      case Right(isTimeout) =>
        (Right(isTimeout), MaxSMTPlay.lastStat)
      case Left((numRemovedConstr, model)) =>
        val newAssignments = ComponentDecoder.decode(model)
        val old = oldExpressions.toMap
        val changes = newAssignments.map({
          case (v, expr) =>
            val s :: i :: Nil = v.name.split("#").tail.map(_.toInt).toList
            (s, i, beautifyResult(old(v.name)), beautifyResult(expr))
        })
        
        (Left(changes), MaxSMTPlay.lastStat)
    }

  }


}
