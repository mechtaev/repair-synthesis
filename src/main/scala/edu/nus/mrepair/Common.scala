package edu.nus.mrepair

import edu.nus.mrepair.synthesis.{ProgramFormula, Formula, ComponentFormula}
import Formula._
import ProgramFormula._
import edu.nus.mrepair.decision._
import edu.nus.vctrans._
import edu.nus.vctrans.ast._
import org.smtlib.command.{C_assert, C_define_fun}
import org.smtlib.IExpr
import edu.nus.mrepair.synthesis.EncodingConfig
import edu.nus.mrepair.ComponentLevel
import edu.nus.mrepair.decision.SolverStat

sealed trait TestData
case class SynthesisAST(formula: ProgramFormulaExpression) extends TestData
case class SMTlibAST(formula: IExpr) extends TestData

case class RepairCondition(hard: List[SolverInputFormula],
                           soft: List[SolverInputFormula])

case class SuspiciousLocations(functions: List[String], globalVariables: Boolean)

sealed trait BackgroundPredicate
case class BackgroundDefinition(definition: DefineFunAst) extends BackgroundPredicate
case class BackgroundDeclaration(definition: DeclareFunAst) extends BackgroundPredicate
case class BackgroundAssertion(assertion: C_assert) extends BackgroundPredicate

case class VCCBenchmark(name: String,
                        version: String,
                        entryFunction: String,
                        smtFile: String,
                        tests: List[List[TestData]],
                        localization: Boolean,
                        suspicious: SuspiciousLocations,
                        globalDecls: List[C_assert],
                        assertions: List[BackgroundPredicate])
                        
case class SynthesisConfig(synthesisConfig: EncodingConfig,
                           simplification: Boolean,
                           reuseStructure: Boolean,
                           spaceReduction: Boolean,
                           solverBound: Int,
                           solverTimeout: Int,
                           componentLevel: ComponentLevel)

case class BenchmarkExecutionResult(benchmark: String,
                                    version: String,
                                    config: SynthesisConfig,
                                    vcProcessingTime: Long,
                                    tfProcessingTime: Long,
                                    rcProcessingTime: Long,
                                    rcSimplificationTime: Long,
                                    rcSolvingTime: Long,
                                    fixed: Boolean,
                                    isTimeout: Boolean,
                                    verified: Boolean,
                                    patch: List[String],
                                    date: String,
                                    localization: Boolean,
                                    locations: Int,
                                    solverStat: SolverStat)

// AngelicFix definitions

object AngelicFix {

  sealed trait VariableValue
  case class IntVal(name: String, value: Int) extends VariableValue
  case class BoolVal(name: String, value: Boolean) extends VariableValue
  case class CharVal(name: String, value: Char) extends VariableValue

  def renameVal(v: VariableValue, newname: String): VariableValue = {
    v match {
      case IntVal(n, v)  => IntVal(newname, v)
      case BoolVal(n, v) => BoolVal(newname, v)
      case CharVal(n, v) => CharVal(newname, v)
    }
  }

  def getName(v: VariableValue): String = {
    v match {
      case IntVal(n, v)  => n
      case BoolVal(n, v) => n
      case CharVal(n, v) => n
    }
  }

  case class AngelicValue(context: List[VariableValue], value: VariableValue, expression: String, instId: Int)

  type AngelicPath = List[AngelicValue]

  type AngelicForest = Map[String, List[AngelicPath]]

}


