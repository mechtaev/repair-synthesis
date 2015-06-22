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

  def generate(angelicForest: AngelicForest,
               suspicious: List[(Int, IExpr)],
               repairConfig: SynthesisConfig): (RepairCondition, List[Component]) = {

    def bindingVar(stmtId: Int): String = "suspicious" + stmtId.toString

    //TODO support instances
    val repairableBindings =
      suspicious.map({
        case (stmtId, expr) =>
          //TODO check type here:
          val Some(pfe) = VCCUtils.translateIfRepairable(expr, { case _ => Some(IntegerType()) })
          (ProgramVariable(bindingVar(stmtId), IntegerType()), pfe, None, stmtId, 1)
      })

    val (repairableObjects, extractedComponents) = 
      extractRepairableObjects(repairableBindings, repairConfig.synthesisConfig, repairConfig.componentLevel)

    //should select components somehow (existing + additional + shared)
    val sharedComponents: List[Component] = Nil

    val (softStructureExpr, hardStructureExpr) = generateStructureConstraints(repairableObjects, repairConfig.synthesisConfig)

    val (softStructure, hardStructure) = (softStructureExpr.map(FormulaAST), hardStructureExpr.map(FormulaAST))

    //TODO if there are multiple suspicious expressions, context for them can have same variables that can have different values
    val semanticsConstraints = angelicForest.values.zipWithIndex.map({
      case (ap, testId) =>
        val start: ProgramFormulaExpression = BooleanValue[ProgramVariable](false)
        val formula = ap.flatten.foldLeft(start)({
          case (acc, AngelicValue(context, value, stmtId)) =>
            val angelic = (ivar(bindingVar(stmtId)) === value)
            val clause = context.foldLeft(angelic)({ case (e, (n, v)) => (ivar(n) === v) & e })
            (clause | acc)
        })

        val synthesisPart = semanticsConstraintsForTestCase(repairableObjects, sharedComponents, testId, repairConfig.synthesisConfig).map(FormulaAST)

        FormulaAST(testConstraintsForTestCase(formula, testId)) :: synthesisPart
    }).flatten


    val components = extractedComponents ++ sharedComponents

    (RepairCondition(hardStructure ++ semanticsConstraints, softStructure), components)
  }


  def solve(rc: RepairCondition,
            components: List[Component],
            repairConfig: SynthesisConfig): (Either[List[(ProgramFormulaExpression, ProgramFormulaExpression)], Boolean], SolverStat) = {

    val RepairCondition(hardClauses, softClauses) = rc

    val solverResult =
      MaxSMTPlay.solve(hardClauses, softClauses, components, repairConfig.simplification, repairConfig.solverBound, repairConfig.solverTimeout)

    solverResult match {
      case Right(isTimeout) =>
        (Right(isTimeout), MaxSMTPlay.lastStat)
      case Left((numRemovedConstr, model)) =>
        val newAssignments = ComponentDecoder.decode(model)
        
        println(newAssignments)
        (Left(List[(ProgramFormulaExpression, ProgramFormulaExpression)]()), MaxSMTPlay.lastStat)
    }

  }


}
