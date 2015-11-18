package edu.nus.mrepair.decision

import edu.nus.mrepair._
import edu.nus.mrepair.synthesis.{ProgramFormula, Formula, ComponentFormula}
import Formula._
import ProgramFormula._
import ComponentFormula._

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.token.StdTokens
import sys.process._

/**
  * Printing SMT-LIB formula for z3-maxsat and parsing output
  */
object Z3Text extends DecisionProcedure {

  def operatorToSMTLIBString(op: Operator): String = {
    op match {
      case Or()             => "or"
      case And()            => "and"
      case Not()            => "not"
      case Equal()          => "="
      case Less()           => "<"
      case LessOrEqual()    => "<="
      case Greater()        => ">"
      case GreaterOrEqual() => ">="
      case Add()            => "+"
      case Sub()            => "-"
      case Mult()           => "*"
      case Div()            => "/"
      case Neg()            => "-"
      case Impl()           => "=>"
      case Iff()            => "iff"
    }
  }

  def printSMTLIBExpression(e: ComponentFormulaExpression) = {
    ComponentFormulaUtils.fold(e, (e: ComponentFormulaExpression) =>
      (l: List[String]) => e match {
        //TODO support ite
        case UninterpretedFunctionApplication(f, _) => "(" + f.toString + l.foldLeft("")(_ + " " + _)  + ")"
        case BinaryOperation(op, _, _) => "(" + operatorToSMTLIBString(op) + " " + l(0) + " " + l(1) + ")"
        case UnaryOperation(op, _) => "(" + operatorToSMTLIBString(op) + " " + l(0) + ")"
        case IntegerValue(v) => v.toString
        case BooleanValue(v) => v.toString
        case v: Variable[ComponentVariable] => ComponentVariableConverter.printVariableName(v.variable)
      })
  }

  def printMutationFormula(hardClauses: List[ComponentFormulaExpression], 
                           softClauses: List[ComponentFormulaExpression]) : String = {
    val declarations = {
      val unique =
         (softClauses.map(ComponentFormulaUtils.collect[Variable[ComponentVariable]]).flatten
           ++ hardClauses.map(ComponentFormulaUtils.collect[Variable[ComponentVariable]]).flatten).map({
             case Variable(v) => (ComponentVariableConverter.printVariableName(v), componentVariableType(v))
           }).distinct
      val integerVars = unique.filter({ case (_, IntegerType()) => true case _ => false }).map(_._1)
      val booleanVars = unique.filter({ case (_, BooleanType()) => true case _ => false }).map(_._1)
      ":extrafuns " + "(" +
        (integerVars.map({
          e => "(" + e + " Int) "
        }) ++
          booleanVars.map({
            e => "(" + e + " Bool) "
          })).foldLeft("")(_ + _) + "\n" + ")"
    }
    val content =
      (softClauses.map({e => ":assumption " + printSMTLIBExpression(e)}) ++
       hardClauses.map({e => ":formula " + printSMTLIBExpression(e)})).foldLeft("")(_ + "\n" + _)
    "(benchmark ex\n :logic UFNIA\n" + declarations + content + ")"
  }

  override def solve(hardClauses: List[SolverInputFormula],
                     softClauses: List[SolverInputFormula],
                     components: List[Component],
                     simplification: Boolean,
                     reuseStructure: Boolean,
                     bound: Int,
                     timeout: Int):
                       Either[(Int, List[(ComponentVariable, Option[Value[ComponentVariable]])]), Boolean] = {
    //FIXME support simplification
    //FIXME support string expressions
    //FIXME support timeout
    //FIXME suport reuseStructure
    val hard = hardClauses.map({ 
      case FormulaAST(formula) => formula
    })
    val soft = softClauses.map({ 
      case FormulaAST(formula) => formula
    })
    //FIXME support bounds in z3 maxsat example
    val smtlibString = printMutationFormula(hard, soft)
    Utils.writeToFile("log/z3text-last-formula.smt", smtlibString)
    "./scripts/solve.sh log/z3text-last-formula.smt".!
    val source = scala.io.Source.fromFile("log/repair.out")
    val content = source.mkString
    source.close()
    val result = ComponentVariableConverter.Z3OutputParser.parse(content, components)
    Utils.writeToFile("log/z3text-last-model.log",
      result.map({case (v, i) => v + " == " + i + "\n"}).foldLeft("")(_ + _))
    //FIXME parse number of removed constraints and return it instead of -1
    //FIXME support solver failure
    Left((-1, result))
  }

}
