package edu.nus.mrepair.synthesis

import Formula._

/**
  * Data structures for representing program expressions that can be repaired
  */
object ProgramFormula {

  case class ProgramVariable(name: String, typ: Type) {
    override def toString: String = {
      name
    }
  }

  object ConstantConversion {
    implicit def intToAST(i: Int): IntegerValue[ProgramVariable] = { IntegerValue(i) }
    implicit def boolToAST(b: Boolean): BooleanValue[ProgramVariable] = { BooleanValue(b) }
  }

  def ivar(name: String): FormulaExpression[ProgramVariable] = 
    Variable(ProgramVariable(name, IntegerType()))

  def bvar(name: String): FormulaExpression[ProgramVariable] = 
    Variable(ProgramVariable(name, BooleanType()))

  object ProgramFormulaUtils extends FormulaUtils[ProgramVariable]

  type ProgramFormulaExpression = FormulaExpression[ProgramVariable]

  object Types {

    def opArgsType(op: Operator): Type = {
      op match {
        case Impl()           => BooleanType()
        case Iff()            => BooleanType()
        case Or()             => BooleanType()
        case And()            => BooleanType()
        case Not()            => BooleanType()
        case Equal()          => IntegerType()
        case Less()           => IntegerType()
        case LessOrEqual()    => IntegerType()
        case Greater()        => IntegerType()
        case GreaterOrEqual() => IntegerType()
        case Add()            => IntegerType()
        case Sub()            => IntegerType()
        case Mult()           => IntegerType()
        case Div()            => IntegerType()
        case Neg()            => IntegerType()
      }
    }

    def opOutputType(op: Operator): Type = {
      op match {
        case Impl()           => BooleanType()
        case Iff()            => BooleanType()
        case Or()             => BooleanType()
        case And()            => BooleanType()
        case Not()            => BooleanType()
        case Equal()          => BooleanType()
        case Less()           => BooleanType()
        case LessOrEqual()    => BooleanType()
        case Greater()        => BooleanType()
        case GreaterOrEqual() => BooleanType()
        case Add()            => IntegerType()
        case Sub()            => IntegerType()
        case Mult()           => IntegerType()
        case Div()            => IntegerType()
        case Neg()            => IntegerType()
      }
    }

    def typeOfExpr(expr: ProgramFormulaExpression): Type = {
      expr match {
        case UninterpretedFunctionApplication(Variable(v), _) => v.typ
        case BinaryOperation(op, _, _) => opOutputType(op)
        case Ite(_, _, _) => IntegerType()
        case UnaryOperation(op, _) => opOutputType(op)
        case IntegerValue(_) => IntegerType()
        case BooleanValue(_) => BooleanType()
        case Variable(ProgramVariable(_, t)) => t
      }
    }

  }

}
