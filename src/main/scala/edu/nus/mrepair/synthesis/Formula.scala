package edu.nus.mrepair.synthesis

import scala.reflect.ClassTag

object Formula {

  /**
    * FormulaExpression type is parametrized by variable type because we use this AST for 
    * both program semantics representation and mutation formula representation (mutation 
    * representation contains additional information about components).
    */
  sealed trait FormulaExpression[V] {
    def +(other: FormulaExpression[V]): BinaryOperation[V]   = BinaryOperation(Equal(), this, other)
    def -(other: FormulaExpression[V]): BinaryOperation[V]   = BinaryOperation(Sub(), this, other)
    def *(other: FormulaExpression[V]): BinaryOperation[V]   = BinaryOperation(Mult(), this, other)
    def /(other: FormulaExpression[V]): BinaryOperation[V]   = BinaryOperation(Div(), this, other)
    def &(other: FormulaExpression[V]): BinaryOperation[V]   = BinaryOperation(And(), this, other)
    def |(other: FormulaExpression[V]): BinaryOperation[V]   = BinaryOperation(Or(), this, other)
    def ===(other: FormulaExpression[V]): BinaryOperation[V] = BinaryOperation(Equal(), this, other)
    def <=>(other: FormulaExpression[V]): BinaryOperation[V] = BinaryOperation(Iff(), this, other)
    def ->(other: FormulaExpression[V]): BinaryOperation[V]  = BinaryOperation(Impl(), this, other)
    def >(other: FormulaExpression[V]): BinaryOperation[V]   = BinaryOperation(Greater(), this, other)
    def >=(other: FormulaExpression[V]): BinaryOperation[V]  = BinaryOperation(GreaterOrEqual(), this, other)
    def <(other: FormulaExpression[V]): BinaryOperation[V]   = BinaryOperation(Less(), this, other)
    def <=(other: FormulaExpression[V]): BinaryOperation[V]  = BinaryOperation(LessOrEqual(), this, other)
    def unary_- = UnaryOperation(Neg(), this)
    def unary_! = UnaryOperation(Not(), this)
  }

  sealed trait Type
  case class IntegerType() extends Type
  case class BooleanType() extends Type
  case class PointerType() extends Type

  //FIXME inconsistency: we use instance of Variable[V] to represent uninterpreted function,
  // however it is not used in the generic traversal functions (fold) because it should be treated differently from variables
  // the type of this variable should represent the output type of the function
  case class UninterpretedFunctionApplication[V](symbol: Variable[V], operands: List[FormulaExpression[V]]) extends FormulaExpression[V] {
    override def toString: String = "(" + symbol.toString + operands.foldLeft("")(_ + " " + _.toString) + ")"
  }
  case class BinaryOperation[V](op: BinaryOperator, l: FormulaExpression[V], r: FormulaExpression[V]) extends FormulaExpression[V] {
    override def toString: String = "(" + l.toString + " "  + op.toString + " " + r.toString + ")"
  }
  case class Ite[V](c: FormulaExpression[V], l: FormulaExpression[V], r: FormulaExpression[V]) extends FormulaExpression[V] {
    override def toString: String = "(if " + c.toString + " then "  + l.toString + " else " + r.toString + ")"
  }
  case class UnaryOperation[V](op: UnaryOperator, expr: FormulaExpression[V]) extends FormulaExpression[V] {
    override def toString: String = "(" + op.toString + " " + expr.toString + ")"
  }
  sealed trait Value[V] extends FormulaExpression[V]
  case class IntegerValue[V](value: Int) extends Value[V] {
    override def toString: String = value.toString
  }
  case class BooleanValue[V](value: Boolean) extends Value[V] {
    override def toString: String = if (value) { "1" } else { "0" }
  }
  case class Variable[V](variable: V) extends FormulaExpression[V] {
    override def toString: String = variable.toString
  }

  class FormulaUtils[V] {

    def fold[T](e: FormulaExpression[V], f: FormulaExpression[V] => List[T] => T) : T = {
      e match {
        case UninterpretedFunctionApplication(symbol, operands) => f(e)(operands.map(fold(_, f)))
        case BinaryOperation(op, l, r) => f(e)(List[T](fold(l, f), fold(r, f)))
        case Ite(c, l, r) => f(e)(List[T](fold(c, f), fold(l, f), fold(r, f)))
        case UnaryOperation(op, v) => f(e)(List[T](fold(v, f)))
        case _: IntegerValue[V] | _: BooleanValue[V] | _: Variable[V] => f(e)(List[T]())
      }
    }

    def collect[T : ClassTag](e: FormulaExpression[V]): List[T] = {
      fold(e, (e: FormulaExpression[V]) => (l: List[List[T]]) =>
        l.flatten ++ (e match {
          case av: T => List(av)
          case _ => List()
        }))
    }

    def substitute[U](expr: FormulaExpression[V],
                      varMapping: V => FormulaExpression[U],
                      ufMapping: V => Variable[U]) : FormulaExpression[U] = {
      fold(expr, (e: FormulaExpression[V]) => (ls: List[FormulaExpression[U]]) =>
        (e, ls) match {
          case (UninterpretedFunctionApplication(Variable(v), _), ops) => UninterpretedFunctionApplication(ufMapping(v), ops)
          case (BinaryOperation(op, _, _), l :: r :: Nil) => BinaryOperation[U](op, l, r)
          case (Ite(_, _, _), c :: l :: r :: Nil) => Ite[U](c, l, r)
          case (UnaryOperation(op, _), l :: Nil) => UnaryOperation[U](op, l)
          case (IntegerValue(v), _) => IntegerValue[U](v)
          case (BooleanValue(v), _) => BooleanValue[U](v)
          case (Variable(variable), _) => varMapping(variable)
        })
    }
  }

  sealed trait Operator extends Product with Serializable
  sealed trait BinaryOperator extends Operator
  sealed trait UnaryOperator extends Operator 
  case class Impl() extends BinaryOperator {
    override def toString: String = "=>"
  }
  case class Iff() extends BinaryOperator {
    override def toString: String = "<=>"
  }
  case class Or() extends BinaryOperator {
    override def toString: String = "||"
  }
  case class And() extends BinaryOperator {
    override def toString: String = "&&"
  }
  case class Not() extends UnaryOperator {
    override def toString: String = "!"
  }
  case class Equal() extends BinaryOperator {
    override def toString: String = "=="
  }
  case class Less() extends BinaryOperator {
    override def toString: String = "<"
  }
  case class LessOrEqual() extends BinaryOperator {
    override def toString: String = "<="
  }
  case class Greater() extends BinaryOperator {
    override def toString: String = ">"
  }
  case class GreaterOrEqual() extends BinaryOperator {
    override def toString: String = ">="
  }
  case class Add() extends BinaryOperator {
    override def toString: String = "+"
  }
  case class Sub() extends BinaryOperator {
    override def toString: String = "-"
  }
  case class Mult() extends BinaryOperator {
    override def toString: String = "*"
  }
  case class Div() extends BinaryOperator {
    override def toString: String = "/"
  }
  case class Neg() extends UnaryOperator {
    override def toString: String = "-"
  }

}
