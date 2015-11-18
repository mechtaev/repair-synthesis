package edu.nus.mrepair.decision

import edu.nus.mrepair._
import edu.nus.mrepair.synthesis.{Formula, ComponentFormula}
import Formula._
import edu.nus.mrepair.synthesis.ComponentFormula
import ComponentFormula._

trait SolverInputFormula
case class FormulaAST(formula: ComponentFormulaExpression) extends SolverInputFormula
case class FormulaString(formula: String) extends SolverInputFormula


/**
  * Interface to MaxSMT solver
  */
trait DecisionProcedure {

  /**
    * Solve partial MaxSMT problem
    * @param hardClauses list of hard clauses
    * @param softClauses list of soft clauses
    * @return number of removed constraints and model
    */
  def solve(hardClauses: List[SolverInputFormula],
            softClauses: List[SolverInputFormula],
            components: List[Component],
            simplification: Boolean,
            reuseStructure: Boolean,
            bound: Int,
            timeout: Int):
              Either[(Int, List[(ComponentVariable, Option[Value[ComponentVariable]])]), Boolean]

  def solveAST(hardClauses: List[ComponentFormulaExpression],
               softClauses: List[ComponentFormulaExpression],
               components: List[Component],
               simplification: Boolean,
               reuseStructure: Boolean,
               bound: Int,
               timeout: Int):
              Either[(Int, List[(ComponentVariable, Option[Value[ComponentVariable]])]), Boolean] = {
    solve(hardClauses.map(FormulaAST), softClauses.map(FormulaAST), components, simplification, reuseStructure, bound, timeout)
  }

}
