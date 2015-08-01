package edu.nus.mrepair.synthesis

import Formula._
import ProgramFormula._
import ComponentFormula._

/**
  * Predefined components
  */
object ComponentLibrary {

  object Standard {

    private val x = ProgramVariable("x", IntegerType())
    private val y = ProgramVariable("y", IntegerType())

    private val a = ProgramVariable("a", BooleanType())
    private val b = ProgramVariable("b", BooleanType())

    def impl    = componentByExpression(BinaryOperation(Impl(), Variable(a), Variable(b)))
    def iff     = componentByExpression(BinaryOperation(Iff(), Variable(a), Variable(b)))
    def or      = componentByExpression(BinaryOperation(Or(), Variable(a), Variable(b)))
    def and     = componentByExpression(BinaryOperation(And(), Variable(a), Variable(b)))
    def not     = componentByExpression(UnaryOperation(Not(), Variable(a)))

    def equal   = componentByExpression(BinaryOperation(Equal(), Variable(x), Variable(y)))
    def less    = componentByExpression(BinaryOperation(Less(), Variable(x), Variable(y)))
    def leq     = componentByExpression(BinaryOperation(LessOrEqual(), Variable(x), Variable(y)))
    def greater = componentByExpression(BinaryOperation(Greater(), Variable(x), Variable(y)))
    def geq     = componentByExpression(BinaryOperation(GreaterOrEqual(), Variable(x), Variable(y)))
    def add     = componentByExpression(BinaryOperation(Add(), Variable(x), Variable(y)))
    def sub     = componentByExpression(BinaryOperation(Sub(), Variable(x), Variable(y)))
    def mult    = componentByExpression(BinaryOperation(Mult(), Variable(x), Variable(y)))
    def div     = componentByExpression(BinaryOperation(Div(), Variable(x), Variable(y)))
    def neg     = componentByExpression(UnaryOperation(Neg(), Variable(x)))

    def ite     = componentByExpression(Ite(Variable(a), Variable(x), Variable(y)))

    def int2bool = componentByExpression(UnaryOperation(Not(), BinaryOperation(Equal(), Variable(x), IntegerValue(0))))

  }

  def componentByOp(op: Operator): FunctionComponent = {
    op match {
      case Impl()           => Standard.impl
      case Iff()            => Standard.iff
      case Or()             => Standard.or
      case And()            => Standard.and
      case Not()            => Standard.not
      case Equal()          => Standard.equal
      case Less()           => Standard.less
      case LessOrEqual()    => Standard.leq
      case Greater()        => Standard.greater
      case GreaterOrEqual() => Standard.geq
      case Add()            => Standard.add
      case Sub()            => Standard.sub
      case Mult()           => Standard.mult
      case Div()            => Standard.div
      case Neg()            => Standard.neg
    }
  }

  def componentByFunctionSymbol(f: ProgramVariable, operandsTypes: List[Type]): FunctionComponent = {
    val operands = operandsTypes.zipWithIndex.map({
      case (t, i) => ProgramVariable("v" + i, t)
    })
    FunctionComponent(operands, f.typ, UninterpretedFunctionApplication(Variable[ProgramVariable](f), operands.map(Variable[ProgramVariable])))
  }

  def altOps(op: BinaryOperator): List[BinaryOperator] = {
    op match {
      case Impl()           => Nil
      case Iff()            => Nil
      case Or()             => And() :: Nil
      case And()            => Or() :: Nil
      case Equal()          => Nil
      case Less()           => LessOrEqual() :: Nil
      case LessOrEqual()    => Less() :: Nil
      case Greater()        => GreaterOrEqual() :: Nil
      case GreaterOrEqual() => Greater() :: Nil
      case Add()            => Sub() :: Nil
      case Sub()            => Add() :: Nil
      case Mult()           => Nil //FIXME add div, but 0 should be considered
      case Div()            => Mult() :: Nil
    }
  }

}
