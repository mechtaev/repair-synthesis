package edu.nus.mrepair.decision

import edu.nus.mrepair.synthesis.{Formula, ComponentFormula}
import edu.nus.mrepair.Utils
import Formula._
import edu.nus.mrepair.synthesis.ComponentFormula
import ComponentFormula._
import edu.nus.mrepair.Utils.SimpleLogger._
import scala.collection.JavaConverters._
import scala.collection.mutable.HashMap
import com.microsoft.z3._
import edu.nus.maxsmtplay._
import edu.nus.mrepair.synthesis.ProgramFormula.Types

case class SolverStat(unsatCores: List[Int],
                      soft: Int,
                      hard: Int)

object MaxSMTPlay extends DecisionProcedure {
  val vars: HashMap[String, Expr] = HashMap()

  var lastStat: SolverStat = null

  def exprToZ3AST(z3: Context, expr: ComponentFormulaExpression): BoolExpr = {
    ComponentFormulaUtils.fold[Expr](expr, {
      val uncurried: (ComponentFormulaExpression, List[Expr]) => Expr = {
        case (BinaryOperation(Impl(), _, _), List(l, r)) =>
          z3.mkImplies(l.asInstanceOf[BoolExpr], r.asInstanceOf[BoolExpr])
        case (BinaryOperation(Or(), _, _), List(l, r)) =>
          z3.mkOr(l.asInstanceOf[BoolExpr], r.asInstanceOf[BoolExpr])
        case (BinaryOperation(And(), _, _), List(l, r)) =>
          z3.mkAnd(l.asInstanceOf[BoolExpr], r.asInstanceOf[BoolExpr])
        case (Ite(_, _, _), List(c, l, r)) =>
          z3.mkITE(c.asInstanceOf[BoolExpr], l.asInstanceOf[ArithExpr], r.asInstanceOf[ArithExpr])
        case (BinaryOperation(Iff(), _, _), List(l, r)) =>
          z3.mkIff(l.asInstanceOf[BoolExpr], r.asInstanceOf[BoolExpr])
        case (BinaryOperation(Equal(), _, _), List(l, r)) =>
//          println("l: " + l + " r: " + r)
          z3.mkEq(l.asInstanceOf[Expr], r.asInstanceOf[Expr])
        case (BinaryOperation(Less(), _, _), List(l, r)) =>
          z3.mkLt(l.asInstanceOf[ArithExpr], r.asInstanceOf[ArithExpr])
        case (BinaryOperation(LessOrEqual(), _, _), List(l, r)) =>
          z3.mkLe(l.asInstanceOf[ArithExpr], r.asInstanceOf[ArithExpr])
        case (BinaryOperation(Greater(), _, _), List(l, r)) =>
          z3.mkGt(l.asInstanceOf[ArithExpr], r.asInstanceOf[ArithExpr])
        case (BinaryOperation(GreaterOrEqual(), _, _), List(l, r)) =>
          z3.mkGe(l.asInstanceOf[ArithExpr], r.asInstanceOf[ArithExpr])
        case (BinaryOperation(Add(), _, _), List(l, r)) =>
          z3.mkAdd(l.asInstanceOf[ArithExpr], r.asInstanceOf[ArithExpr])
        case (BinaryOperation(Sub(), _, _), List(l, r)) =>
          z3.mkSub(l.asInstanceOf[ArithExpr], r.asInstanceOf[ArithExpr])
        case (BinaryOperation(Mult(), _, _), List(l, r)) =>
          z3.mkMul(l.asInstanceOf[ArithExpr], r.asInstanceOf[ArithExpr])
        case (BinaryOperation(Div(), _, _), List(l, r)) =>
          z3.mkDiv(l.asInstanceOf[ArithExpr], r.asInstanceOf[ArithExpr])
        case (UnaryOperation(Not(), _), List(v)) =>
          z3.mkNot(v.asInstanceOf[BoolExpr])
        case (UnaryOperation(Neg(), _), List(v)) =>
          z3.mkUnaryMinus(v.asInstanceOf[ArithExpr])
        case (IntegerValue(i), _) =>
          z3.mkInt(i)
        case (BooleanValue(i), _) =>
          z3.mkBool(i)
        case (UninterpretedFunctionApplication(v, args), ops) =>
          val name = ComponentVariableConverter.printVariableName(v.variable)
          val decl = z3.mkFuncDecl(name, z3.getIntSort, z3.getIntSort) //FIXME detect type
          z3.mkApp(decl, ops:_*)
        case (v: Variable[ComponentVariable], _) =>
          val name = ComponentVariableConverter.printVariableName(v.variable)
          val z3var = if (vars.contains(name)) {
            vars(name)
          } else {
            val newvar = componentVariableType(v.variable) match {
              case IntegerType() =>
                z3.mkIntConst(name)
              case BooleanType() =>
                z3.mkBoolConst(name)
              case PointerType() =>
                //FIXME hardcoded string
                val ptrSort = z3.mkUninterpretedSort("T@$ptr")
                z3.mkConst(name, ptrSort)
            }
            vars += name -> newvar
            newvar
          }
          z3var
      }
      uncurried.curried
    }).asInstanceOf[BoolExpr]
  }

  def evalVariables(components: List[Component],
                    z3: Context,
                    model: Model): List[(ComponentVariable, Option[Value[ComponentVariable]])] = {
    vars.keys.toList.map({ case name =>
      val variable = ComponentVariableConverter.Z3OutputParser.parseVariable(name, components)
      val value = 
        componentVariableType(variable) match {
          case IntegerType() =>
            val evalResult = model.eval(z3.mkIntConst(name), false)
            try {
              val value = evalResult.asInstanceOf[IntNum].getInt
              Some(IntegerValue[ComponentVariable](value))
            } catch {
              case _ => None
            }
          case BooleanType() =>
            val evalResult = model.eval(z3.mkBoolConst(name), false)
            try {
              val value = evalResult.isTrue
              Some(BooleanValue[ComponentVariable](value))
            } catch {
              case _ => None
            }
          case PointerType() =>
            if (Utils.verbose) println("[warn] solver: evaluating pointer value " + name)
            None
        }
      (variable, value)
    })
  }

  override def solve(hardClauses: List[SolverInputFormula],
                     softClauses: List[SolverInputFormula],
                     components: List[Component],
                     simplification: Boolean,
                     reuseStructure: Boolean,
                     bound: Int,
                     timeout: Int):
                       Either[(Int, List[(ComponentVariable, Option[Value[ComponentVariable]])]), Boolean] = {
    val solver = if (reuseStructure) {
      new FuMalik(Some(bound)) with Circuit with Z3
    } else {
      new Sat() with Z3
    }

    solver.init(Some(timeout))

    vars.clear()
    val hard = hardClauses.map({
      case FormulaAST(formula) => exprToZ3AST(solver.z3, formula)
      case FormulaString(formula) =>
        if (Utils.enableLogging) formula.toString.log("maxsmt-parsing.log")
        solver.z3.parseSMTLIB2String(formula, Array(), Array(), Array(), Array())
    })
    val soft = softClauses.map({
      case FormulaAST(formula) => exprToZ3AST(solver.z3, formula)
      case FormulaString(formula) =>
        solver.z3.parseSMTLIB2String(formula, Array(), Array(), Array(), Array())
    })

    Utils.logTime // simplifying hard constraints


    val processedHard = 
      if (simplification) {
        if (Utils.verbose) println("[info] simplifying formula...")

        val goal = solver.z3.mkGoal(true, true, false) //TODO clarify these parameters
        hard.map({ case h => goal.add(h) })
        val simp: Tactic = solver.z3.mkTactic("simplify")
        val prop: Tactic = solver.z3.mkTactic("propagate-values")
        val ctx_simp: Tactic = solver.z3.mkTactic("ctx-simplify")
        //    val css: Tactic = solver.z3.mkTactic("ctx-solver-simplify")
        val simplification = solver.z3.then(simp, prop, ctx_simp)
        val res = simplification.apply(goal)
        val simplifiedHard = res.getSubgoals()(0).getFormulas().toList
        // if (Utils.enableLogging)
        //   simplifiedHard.map(_.toString + "\n").fold("")(_ + _).log("maxsmt-hard-nonsimplified.log")
        simplifiedHard
      } else {
        hard
      }

    if (Utils.enableLogging) soft.map(_.toString + "\n").fold("")(_ + _).log("maxsmt-soft.log")
    if (Utils.enableLogging) processedHard.map(_.toString + "\n").fold("")(_ + _).log("maxsmt-hard.log")

    Utils.logTime // solving rc

    if (Utils.verbose) println("[info] solving formula...")
    val result = solver.solveAndGetModel(soft, processedHard)
    lastStat = SolverStat(unsatCores = Nil,//solver.lastCores,
                          hard = hardClauses.size,
                          soft = softClauses.size)
    val isTimeout = solver.solver.getReasonUnknown().equals("canceled")
    result match {
      case None =>
        solver.delete()
        Right(isTimeout)
      case Some((clauses, model)) =>
        val locations = evalVariables(components, solver.z3, model)
        solver.delete()
        Left((soft.size + hard.size - clauses.size, locations))
    }
  }
}
