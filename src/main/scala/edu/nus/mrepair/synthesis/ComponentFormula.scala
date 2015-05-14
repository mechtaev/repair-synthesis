package edu.nus.mrepair.synthesis

import Formula._
import ProgramFormula._

/** 
  * Component presentation and its formula representation data structures
  */
object ComponentFormula {

  object IdGenerator {
    var last = 0
    def generate(): Int = {
      last = last + 1
      last
    }
  }

  /** 
    * Components used for synthesis. Each component has its unique id.
    * FunctionComponent stands for operations or subexpressions (or methods calls?).
    * VariableComponent corresponds to program variables.
    * IntegerConstantComponent and BooleanConstantComponent correspond to constants
    */
  sealed trait Component {
    val id = IdGenerator.generate()
  }

  sealed trait MultiInputComponent extends Component

  case class FunctionComponent(inputs: List[ProgramVariable],
                               output: Type,
                               semantics: ProgramFormulaExpression) extends MultiInputComponent {
    override def toString: String = "id" + this.id + "(" + semantics.toString + ")"
  }
  case class VariableComponent(variable: ProgramVariable) extends Component {
    override def toString: String = "id" + this.id + "(" + variable.name + ")"
  }
  case class IntegerConstantComponent() extends Component {
    override def toString: String = "id" + this.id + "(int)"
  }
  case class BooleanConstantComponent() extends Component {
    override def toString: String = "id" + this.id + "(bool)"
  }
  //TODO do we need the insformation about outputs here?
  // fake components that are used to simplify creation of constraints to bind several instances of statements
  case class PhantomComponent(inputs: List[ProgramVariable], //need variables here because of the soft and hard bindings
                              output: Type) extends MultiInputComponent {
    override def toString: String = "id" + this.id + "(fake)"
  }

  def componentType(c: Component): Type = {
    c match {
      case f: FunctionComponent => f.output
      case v: VariableComponent => v.variable.typ
      case i: IntegerConstantComponent => IntegerType()
      case i: BooleanConstantComponent => BooleanType()
      case p: PhantomComponent => p.output
    }
  }

  def componentByExpression(expr: ProgramFormulaExpression): FunctionComponent = {
    val vars = ProgramFormulaUtils.collect[Variable[ProgramVariable]](expr).map({ case Variable(v) => v }).distinct
    FunctionComponent(vars, Types.typeOfExpr(expr), expr)
  }

  sealed trait ComponentVariable

  /** 
    * There are tree types of component formula variables:
    * - Location variables
    * - Instances of ConnectionVariable
    * - Separator variables
    */
  case class Location(v: InstanceWithLocation) extends ComponentVariable {
    override def toString: String = "loc" + v.toString
  }

  // separators between components corresponding to different expressions
  case class SeparatorVariable(index: Int, gropuid: Int) extends ComponentVariable

  sealed trait Instance extends ComponentVariable

  case class TestInstance(v: ConnectionVariable, testId: Int) extends Instance {
    override def toString: String = "[test=" + testId + "](" + v.toString + ")"
  }

  sealed trait InstanceWithLocation extends Instance

  case class StatementInstance(v: ConnectionVariable, stmtId: Int) extends InstanceWithLocation {
    override def toString: String = "[stmt=" + stmtId + "](" + v.toString + ")"
  }
  case class ExecutionInstance(v: ConnectionVariable, stmtId: Int, exeId: Int) extends InstanceWithLocation {
    override def toString: String = "[stmt=" + stmtId + ",exe=" + exeId + "](" + v.toString + ")"
  }

  case class TestExecutionInstance(v: ConnectionVariable, testId: Int, stmtId: Int, exeId: Int) extends Instance {
    override def toString: String = "[stmt=" + stmtId + ",test=" + testId + ",exe=" + exeId + "](" + v.toString + ")"
  }

  /** 
    * Connection variables represent values of inputs and outputs of components and output bindings
    */
  sealed trait ConnectionVariable
  case class ComponentInput(comp: MultiInputComponent, inputIndex: Int) extends ConnectionVariable {
    override def toString: String = "in" + inputIndex + "(" + comp.toString + ")"
  }
  case class ComponentOutput(comp: Component) extends ConnectionVariable {
    override def toString: String = "out(" + comp.toString + ")"
  }

  /** 
    * This variable is considered as an input, but compared to other inputs it is used as a test 
    * instance instead of a local instance because its values are shared between several statements.
    */
  case class BindingVariable(v: ProgramVariable) extends ConnectionVariable

  object ConstantConversion {
    implicit def intToAST(i: Int): IntegerValue[ComponentVariable] = { IntegerValue(i) }
    implicit def boolToAST(b: Boolean): BooleanValue[ComponentVariable] = { BooleanValue(b) }
  }

  type ComponentFormulaExpression = FormulaExpression[ComponentVariable]

  object ComponentFormulaUtils extends FormulaUtils[ComponentVariable]

  def componentVariableType(cv: ComponentVariable): Type = {
    cv match {
      case Location(_) => IntegerType()
      case TestInstance(v, _) => connectionVariableType(v)
      case StatementInstance(v, _) => connectionVariableType(v)
      case ExecutionInstance(v, _, _) => connectionVariableType(v)
      case TestExecutionInstance(v, _, _, _) => connectionVariableType(v)
      case SeparatorVariable(_, _) => IntegerType()
    }
  }

  def connectionVariableType(cv: ConnectionVariable): Type = {
    cv match {
      case ComponentOutput(c) => componentType(c)
      case ComponentInput(c, index) =>
        c match {
          case cmp: FunctionComponent => cmp.inputs(index).typ
          case cmp: PhantomComponent => cmp.inputs(index).typ
        }
      case BindingVariable(v) => v.typ
    }
  }

  /** 
    * Representing given expression as a set of components and their connections.
    * Connections are mappings from inputs indexes to other components for function
    * components and values from corresponding domains for symbolic components. In contrast
    * to soft constraints, hard connections cannot be modified by the repair algorithm.
    */
  sealed trait ComponentPresentation

  case class FunctionComponentNode(component: Component,
                                   alternatives: List[Component],
                                   soft: Map[ProgramVariable, ComponentPresentation],
                                   hard: Map[ProgramVariable, ComponentPresentation]) extends ComponentPresentation
  case class VariableComponentNode(component: Component) extends ComponentPresentation
  case class BooleanConstantComponentNode(component: Component,
                                          value: Boolean,
                                          isSoft: Boolean) extends ComponentPresentation
  case class IntegerConstantComponentNode(component: Component,
                                          value: Int,
                                          isSoft: Boolean) extends ComponentPresentation
}
