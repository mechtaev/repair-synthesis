package edu.nus.mrepair

import edu.nus.mrepair.synthesis._
import Formula._
import ProgramFormula._
import ComponentFormula._
import ComponentFormula.ConstantConversion._
import edu.nus.mrepair.Utils.SimpleLogger._
import edu.nus.mrepair.Utils._

sealed trait ComponentLevel
case class Alternatives() extends ComponentLevel
case class IntegerConstants() extends ComponentLevel
case class BooleanConstants() extends ComponentLevel
case class Variables() extends ComponentLevel
case class BasicArithmetic() extends ComponentLevel
case class BasicLogic() extends ComponentLevel
case class BasicInequalities() extends ComponentLevel
case class ExtendedArithmetic() extends ComponentLevel
case class ExtendedLogic() extends ComponentLevel
case class ExtendedInequalities() extends ComponentLevel
case class MixedConditional() extends ComponentLevel
case class ConditionalArithmetic() extends ComponentLevel

object StatementLevelRepair {

  //FIXME magic number
  val phantomExeId = 123456

  //assigned variable, extracted components, additional components, presentations
  type Binding = (BindingVariable, List[Component], List[Component], ComponentPresentation)

  sealed trait RepairableObject
  case class SingleStatement(stmtId: Int, bound: Option[Int], binding: Binding) extends RepairableObject
  case class StatementInstances(stmtId: Int, bound: Option[Int], bindings: Map[Int, Binding]) extends RepairableObject


  type RepairableBindings = List[(ProgramVariable, ProgramFormulaExpression, Option[Int], Int, Int)]

  // returns presentation, components that are used in presentation and additional components
  private def splitAndSelectComponents(expr: ProgramFormulaExpression,
                                       stmtId: Int,
                                       exeId: Int, 
                                       config: EncodingConfig,
                                       componentLevel: ComponentLevel): (ComponentPresentation, List[Component], List[Component]) = {
    var components = List[Component]()

    def bindVariables(expr: ProgramFormulaExpression):
        (Map[ProgramVariable, ComponentPresentation], List[Component]) = {
      val vars =
        ProgramFormulaUtils.collect[Variable[ProgramVariable]](expr).map({ case Variable(v) => v }).distinct
      val components = vars.map({ case v => VariableComponent(v) })
      val presentations = components.map({ case c => VariableComponentNode(c)})
      (vars.zip(presentations).toMap, components)
    }

    def deepExpressionComponent(expr: ProgramFormulaExpression): FunctionComponentNode = {
      val component = componentByExpression(expr)
      val (bindings, varsComponents) = bindVariables(expr)
      components = (component :: varsComponents) ++ components
      FunctionComponentNode(component, List(), Map(), bindings)
    }

    def shallowUninterpretedFunctionComponent(uf: ProgramVariable, 
                                              operandsTypes: List[Type], 
                                              operands: List[ComponentPresentation]): FunctionComponentNode = {
      val component = ComponentLibrary.componentByFunctionSymbol(uf, operandsTypes)
      components = component :: components
      val connections = component.inputs.zip(operands).toMap
      FunctionComponentNode(component, Nil, connections, Map())
    }

    def shallowBinaryOperationComponent(op: BinaryOperator, 
                                        operands: List[ComponentPresentation]): FunctionComponentNode = {
      val component = ComponentLibrary.componentByOp(op)
      def alts: List[FunctionComponent] = ComponentLibrary.altOps(op).map(ComponentLibrary.componentByOp)
      val alternatives = (componentLevel, op) match {
        case (Alternatives(), _) => alts
        case _ => Nil
      }
      components = (component :: alternatives) ++ components
      val connections = component.inputs.zip(operands).toMap
      FunctionComponentNode(component, alternatives, connections, Map())
    }

    def shallowIteComponent(operands: List[ComponentPresentation]): FunctionComponentNode = {
      val component = ComponentLibrary.Standard.ite
      components = component :: components
      val connections = component.inputs.zip(operands).toMap
      FunctionComponentNode(component, Nil, connections, Map())
    }

    def shallowUnaryOperationComponent(op: UnaryOperator, 
                                       operand: ComponentPresentation): FunctionComponentNode = {
      val component = ComponentLibrary.componentByOp(op)
      components = component :: components
      val connections = component.inputs.zip(operand :: Nil).toMap
      FunctionComponentNode(component, List(), connections, Map())
    }


    def inner(e: ProgramFormulaExpression) : ComponentPresentation = {
      e match {
        case UninterpretedFunctionApplication(uf, operands) =>
          deepExpressionComponent(e)
          // val operandsTypes = operands.map(Types.typeOfExpr)
          // config.level match {
          //   case Logical() =>
          //     //TODO does it makes sense to add negation here?
          //     uf.variable.typ match {
          //       case IntegerType() => deepExpressionComponent(e)
          //       case BooleanType() =>
          //         val hasIntegerOperand = operandsTypes.map(_ == IntegerType()).foldLeft(false)(_ || _)
          //         if (hasIntegerOperand) {
          //           deepExpressionComponent(e)
          //         } else {
          //           shallowUninterpretedFunctionComponent(uf.variable, operandsTypes, operands.map(inner))
          //         }
          //     }
          //   case Linear() => shallowUninterpretedFunctionComponent(uf.variable, operandsTypes, operands.map(inner))
          //   case NonLinear() => shallowUninterpretedFunctionComponent(uf.variable, operandsTypes, operands.map(inner))
          // }
        case BinaryOperation(op, l, r) =>
          config.level match {
            case Logical() =>
              Types.opOutputType(op) match {
                case BooleanType() =>
                  val lt = Types.typeOfExpr(l)
                  val rt = Types.typeOfExpr(r)
                    (lt, rt) match {
                    case (BooleanType(), BooleanType()) =>
                      shallowBinaryOperationComponent(op, inner(l) :: inner(r) :: Nil)
                    case _ => deepExpressionComponent(e)
                  }
                case _ => deepExpressionComponent(e)
              }
            case Linear() =>
              op match {
                case Mult() | Div() => deepExpressionComponent(e)
                case _ => shallowBinaryOperationComponent(op, inner(l) :: inner(r) :: Nil)
              }
            case NonLinear() => shallowBinaryOperationComponent(op, inner(l) :: inner(r) :: Nil)
          }
        case Ite(c, l, r) =>
          config.level match {
            case Logical() =>
              shallowIteComponent(inner(c) :: deepExpressionComponent(l) :: deepExpressionComponent(r) :: Nil)
            case Linear() =>
              shallowIteComponent(inner(c) :: inner(l) :: inner(r) :: Nil)
            case NonLinear() =>
              shallowIteComponent(inner(c) :: inner(l) :: inner(r) :: Nil)
          }
        case UnaryOperation(op, x) =>
          config.level match {
            case Logical() =>
              op match {
                case Neg() => deepExpressionComponent(e)
                case Not() => shallowUnaryOperationComponent(op, inner(x))
              }
            case Linear() => shallowUnaryOperationComponent(op, inner(x))
            case NonLinear() => shallowUnaryOperationComponent(op, inner(x))
          }
        case IntegerValue(i) =>
          val component = IntegerConstantComponent()
          components = component :: components
          IntegerConstantComponentNode(component, i, config.repairIntegerConst)
        case BooleanValue(b) =>
          val component = BooleanConstantComponent()
          components = component :: components
          BooleanConstantComponentNode(component, b, config.repairIntegerConst)
        case Variable(v) =>
          val component = VariableComponent(v)
          components = component :: components
          VariableComponentNode(component)
      }
    }

    val representation = inner(expr)

//    val variablesForThisExpr = VariableComponentSelector.select(stmtId, exeId)
//    println("[info] for statement " + stmtId + " instance " + exeId + " using components: " + variablesForThisExpr)

    //this is only for instances:
    //val additionalComponents = if (exeId == 0) Nil else selectAdditionalComponents(componentLevel, stmtId, exeId)

    val additionalComponents = selectAdditionalComponents(componentLevel, stmtId, exeId)

    if (Utils.enableLogging) prettyList(additionalComponents).log("rc-additional-components-s" + stmtId + "-e" + exeId + ".log")

    (representation, components, additionalComponents)
  }

  def getSharedComponents(level: ComponentLevel): List[Component] = {
    ???
  }

  def selectAdditionalComponents(level: ComponentLevel, stmtId: Int, exeId: Int): List[Component] = {
    level match {
      case Alternatives() => Nil
      case IntegerConstants() => IntegerConstantComponent() :: Nil
      case BooleanConstants() => BooleanConstantComponent() :: Nil
      case Variables() => VariableComponentSelector.select(stmtId, exeId)
      case BasicArithmetic() =>
        IntegerConstantComponent() :: (Add() :: Neg() :: Nil).map(ComponentLibrary.componentByOp)
      case BasicLogic() =>
        ComponentLibrary.Standard.int2bool :: (And() :: Not() :: Nil).map(ComponentLibrary.componentByOp)
      case BasicInequalities() =>
        IntegerConstantComponent() :: (Equal() :: Greater() :: GreaterOrEqual() :: Nil).map(ComponentLibrary.componentByOp)
      case ExtendedArithmetic() =>
        IntegerConstantComponent() :: VariableComponentSelector.select(stmtId, exeId) ++ (Add() :: Neg() :: Nil).map(ComponentLibrary.componentByOp)
      case ExtendedLogic() =>
        ComponentLibrary.Standard.int2bool :: VariableComponentSelector.select(stmtId, exeId) ++ (And() :: Not() :: Nil).map(ComponentLibrary.componentByOp)
      case ExtendedInequalities() =>
        IntegerConstantComponent() :: VariableComponentSelector.select(stmtId, exeId) ++ (Equal() :: Greater() :: GreaterOrEqual() :: Nil).map(ComponentLibrary.componentByOp)
      case MixedConditional() =>
        IntegerConstantComponent() :: VariableComponentSelector.select(stmtId, exeId) ++ (Greater() :: GreaterOrEqual() :: Nil).map(ComponentLibrary.componentByOp) ++ (And() :: Not() :: Nil).map(ComponentLibrary.componentByOp) ++ (Add() :: Neg() :: Nil).map(ComponentLibrary.componentByOp)
      case ConditionalArithmetic() =>
        ComponentLibrary.Standard.ite :: IntegerConstantComponent() :: VariableComponentSelector.select(stmtId, exeId) ++ (Greater() :: GreaterOrEqual() :: Nil).map(ComponentLibrary.componentByOp) ++ (Add() :: Neg() :: Nil).map(ComponentLibrary.componentByOp)
    }
  }

  /** Generate constraints that capture semantics. Supposed to be used as hard.
    */
  def semanticsConstraintsForTestCase(repairableObjects: List[RepairableObject],
                                      globalComponents: List[Component],
                                      testId: Int,
                                      config: EncodingConfig): List[ComponentFormulaExpression] = {
    var group = 0
    val encoder = new ComponentEncoder(config.componentsMultipleOccurrences)
    if (config.phantomComponents) {
      val singleExpressionComponents = repairableObjects.map({
        case SingleStatement(stmtId, bound, (v, comps, additional, pres)) =>(v, bound, stmtId, 0, comps ++ additional) :: Nil
        case StatementInstances(stmtId, bound, bindings)      => Nil
      }).flatten
      val instancesExpressionComponents = repairableObjects.map({
        case SingleStatement(stmtId, bound, (v, comps, additional, pres)) => Nil
        case StatementInstances(stmtId, bound, bindings) =>
          bindings.map({ case (exe, (v, comps, additional, pres)) => (v, bound, stmtId, exe, comps ++ additional)})
      })
      group = group + 1
      val single = encoder.encodeSemantics(singleExpressionComponents, globalComponents, group, testId, config)
      val instances = instancesExpressionComponents.map({
        //TODO should I put additional components here?
        case exprComps =>
          //should be encoded separatly because all locations are bound
          exprComps.map({
            case ec =>           
              group = group + 1
              encoder.encodeSemantics(ec :: Nil, Nil, group, testId, config)
          }).flatten
      }).flatten
      single ++ instances
    } else {
      val expressionComponents = repairableObjects.map({
        case SingleStatement(stmtId, bound, (v, comps, additional, pres)) =>
          (v, bound, stmtId, 0, comps ++ additional) :: Nil
        case StatementInstances(stmtId, bound, bindings) =>
          bindings.map({ case (exe, (v, comps, additional, pres)) => (v, bound, stmtId, exe, comps ++ additional)})
      }).flatten
      encoder.encodeSemantics(expressionComponents, globalComponents, group, testId, config)
    }
  }

  /** Generate constraints that capture expected behaviour. Supposed to be used as hard.
    */
  def testConstraintsForTestCase(testCase: ProgramFormulaExpression, testId: Int): ComponentFormulaExpression = {
    ProgramFormulaUtils.substitute[ComponentVariable](
      testCase,
      { case v => Variable(TestInstance(BindingVariable(v), testId)) },
      { case v => Variable(TestInstance(BindingVariable(v), testId)) })
  }

  /** Generate constraints that capture structure.
    * @return soft and hard constraints
    */
  def structureConstraints(variable: BindingVariable,
                           stmtId: Int,
                           exeId: Int,
                           presentation: ComponentPresentation,
                           config: EncodingConfig): (List[ComponentFormulaExpression], List[ComponentFormulaExpression]) = {
    val encoder = new ComponentEncoder(config.componentsMultipleOccurrences)
    encoder.encodeStructure(presentation, variable, stmtId, exeId)
  }

  def constructPhantomBindings(b: Binding): Binding = {
    val (BindingVariable(ProgramVariable(name, typ)), _, additional, pres) = b
    val phantomVariable = BindingVariable(ProgramVariable("phantom" + name, typ))
    def toPhantom(cmp: Component): PhantomComponent = {
      cmp match {
        case FunctionComponent(finputs, foutput, _) => PhantomComponent(inputs = finputs, output = foutput)
        case VariableComponent(v) => PhantomComponent(inputs = Nil, output = v.typ)
        case IntegerConstantComponent() => PhantomComponent(inputs = Nil, output = IntegerType())
        case BooleanConstantComponent() => PhantomComponent(inputs = Nil, output = BooleanType())
      }
    }
    def buildPhantomPres(pres: ComponentPresentation): (ComponentPresentation, List[Component]) = {
      pres match {
        case FunctionComponentNode(component, alternatives, soft, hard) =>
          val newComp = toPhantom(component)
          val newAlts = alternatives.map(toPhantom)
          val (newSoft, newSoftComps) = soft.toList.map({
            case (v, p) =>
              val (newP, newC) = buildPhantomPres(p)
              ((v, newP), newC)
          }).unzip
          val (newHard, newHardComps) = hard.toList.map({
            case (v, p) =>
              val (newP, newC) = buildPhantomPres(p)
              ((v, newP), newC)
          }).unzip
          (FunctionComponentNode(newComp, newAlts, newSoft.toMap, newHard.toMap), (newComp :: newAlts) ++ newHardComps.flatten ++ newSoftComps.flatten)
        case VariableComponentNode(component) =>
          val newComp = toPhantom(component)
          (FunctionComponentNode(newComp, Nil, Map(), Map()), newComp :: Nil)
        case BooleanConstantComponentNode(component, value, isSoft) =>
          assert(!isSoft)
          val newComp = toPhantom(component)
          (FunctionComponentNode(newComp, Nil, Map(), Map()), newComp :: Nil)
        case IntegerConstantComponentNode(component, value, isSoft) =>
          assert(!isSoft)
          val newComp = toPhantom(component)
          (FunctionComponentNode(newComp, Nil, Map(), Map()), newComp :: Nil)
      }
    }
    val (phantomPres, newComps) = buildPhantomPres(pres)
    val phantomAdditional = additional.map(toPhantom)
    (phantomVariable, newComps, phantomAdditional, phantomPres)
  }


  def generateStructureConstraints(repobjs: List[RepairableObject],
                                   config: EncodingConfig): (List[ComponentFormulaExpression], List[ComponentFormulaExpression]) = {
    val (softList, hardList) = repobjs.map({
      case SingleStatement(stmtId, _, (v, _, _, pres)) =>
        structureConstraints(v, stmtId, 0, pres, config)
      case StatementInstances(stmtId, _, bindings) =>
        if (config.phantomComponents) {
          val encoder = new ComponentEncoder(config.componentsMultipleOccurrences)
          val phantomBinding = constructPhantomBindings(bindings.values.head)
          if (Utils.enableLogging) phantomBinding.toString.log("rc-phantom-bindings-s" + stmtId + ".log")
          val hardInstancesStructure = {
            val (_, h) = bindings.map({
              case (exeId, (v, _, _, pres)) => structureConstraints(v, stmtId, exeId, pres, config)
            }).unzip
            h.flatten
          }
          val linkedStructureConstraints =
            bindings.map({ case (exeId, binding) => encoder.linkToPhantom(stmtId, exeId, binding, phantomBinding) }).flatten
          val (s, h) = phantomBinding match {
            case (phantomVar, _, _, phantomPres) =>
              structureConstraints(phantomVar, stmtId, phantomExeId, phantomPres, config)
          }
          (s, h ++ linkedStructureConstraints ++ hardInstancesStructure)
        } else {
          val (s, h) = bindings.map({
            case (exeId, (v, _, _, pres)) => structureConstraints(v, stmtId, exeId, pres, config)
          }).unzip
          (s.flatten, h.flatten)
        }
    }).unzip
    (softList.flatten, hardList.flatten)
  }

  def extractRepairableObjects(rbs: RepairableBindings,
                               config: EncodingConfig,
                               componentLevel: ComponentLevel): (List[RepairableObject], List[Component]) = {
    
    val (exprComponents, exprPresentations) = rbs.map({
      case (v, expr, bound, stmtId, exeId) =>
        val (pres, comps, additional) = splitAndSelectComponents(expr, stmtId, exeId, config, componentLevel)
        (comps ++ additional, (v, bound, stmtId, exeId, comps, additional, pres))
    }).unzip

    val repobjs: List[RepairableObject] = exprPresentations.groupBy({ case (_, _, stmtId, _, _, _, _) => stmtId }).map({
      case (stmtId, prs) =>
        prs match {
          case (v, bound, _, exeId, comps, additional, pres) :: Nil => SingleStatement(stmtId, bound, (BindingVariable(v), comps, additional, pres))
          case l => 
            val bound = l.map({ case (_, b, _, _, _, _, _) => b }).apply(0) //TODO verify that all are the same
            StatementInstances(stmtId, bound, l.map({
              case (v, _, _, exeId, comps, additional, pres) => (exeId, (BindingVariable(v), comps, additional, pres))
            }).toMap)
        }
    }).toList

    (repobjs, exprComponents.flatten)

  }

}
