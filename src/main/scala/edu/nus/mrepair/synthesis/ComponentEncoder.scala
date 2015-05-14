package edu.nus.mrepair.synthesis

import edu.nus.mrepair.StatementLevelRepair.{Binding, StatementInstances, SingleStatement, RepairableObject}
import edu.nus.mrepair._
import Formula._
import ProgramFormula._
import ComponentFormula._
import ComponentFormula.ConstantConversion._

import scala.reflect.ClassTag

sealed trait EncodingLevel
case class Logical() extends EncodingLevel
case class Linear() extends EncodingLevel
case class NonLinear() extends EncodingLevel

case class EncodingConfig(level: EncodingLevel, 
                          repairIntegerConst: Boolean, 
                          repairBooleanConst: Boolean, 
                          componentsMultipleOccurrences: Boolean,
                          phantomComponents: Boolean)

class ComponentEncoder(componentsMultipleOccurrences: Boolean) {

  val globalComponentsStmtId = 0 //FIXME this is a bit ugly
  val globalComponentsExeId = 0 //FIXME this is a bit ugly

  val supportedTypes = IntegerType() :: BooleanType() :: PointerType() :: Nil

  def filterComponentsByType[T: ClassTag](components: List[Component]): List[T] = {
    components.foldLeft(List[T]())({
      case (cs, c) => c match {
        case comp: T => comp :: cs
        case _ => cs
      }
    })
  }

  def collectInputs(c: FunctionComponent): List[ComponentInput] = {
    c.inputs.zipWithIndex.map({ case (_, index) => ComponentInput(c, index) })
  }

  /**
    * Correspond to well-formedness constraints (components range and acyclicity), but without consistency constraints
    */
  def inputsAndOutputsRange(components: List[Component],
                            range: (SeparatorVariable, SeparatorVariable),
                            stmtId: Int,
                            exeId: Int): List[ComponentFormulaExpression] = {
    def L(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(Location(ExecutionInstance(v, stmtId, exeId)))

    val functionComponents = filterComponentsByType[FunctionComponent](components)
    val outputs = components.map(ComponentOutput)
    val inputs =
      functionComponents.map({
        case fc => fc.inputs.zipWithIndex.map({
          case (_, index) => ComponentInput(fc, index)
        })
      }).flatten
    val (beg, end) = range

    val rangeConstraints =
      ((inputs ++ outputs).map({
        case v => (Variable[ComponentVariable](beg) <= L(v)) & (L(v) < Variable[ComponentVariable](end))
      }) ::

      // acyclicity:
      functionComponents.map(comp => {
        val ins = collectInputs(comp)
        val o = ComponentOutput(comp)
        ins.map({ case i => L(i) < L(o) })
      })).flatten

    rangeConstraints
  }

  def encodeSemantics(expressionComponents: List[(BindingVariable, Option[Int], Int, Int, List[Component])],
                      globalComponents: List[Component],
                      groupId: Int,
                      testId: Int,
                      config: EncodingConfig): List[ComponentFormulaExpression] = {
    if (expressionComponents.size == 0) {
      return (BooleanValue[ComponentVariable](true) :: Nil)
    }

      def fvar(v: SeparatorVariable): ComponentFormulaExpression = Variable[ComponentVariable](v)
    var index = 0
    val beginning = SeparatorVariable(index, groupId)

    val allLocalComponents = expressionComponents.map({ case (_, _, _, _, l) => l }).flatten
    val totalComponentsNum = allLocalComponents.size + globalComponents.size
    var end = beginning
    var separators = beginning :: Nil

    val constraints = expressionComponents.map({
      case (v, bound, stmtId, exeId, components) =>
        val beg = end
        index = index + 1
        end = SeparatorVariable(index, groupId)
        separators = end :: separators
        val result = generateLocalConstraints(components, globalComponents, v, (beg, end), bound, testId, stmtId, exeId)
        (fvar(beg) <= fvar(end)) :: result
    }).flatten

    val beginningConstraint = fvar(beginning) === 0
    val endConstraint = (fvar(end) === totalComponentsNum)

    val globalComponentsConstraints =
       (inputsAndOutputsRange(globalComponents, (beginning, end), globalComponentsStmtId, globalComponentsExeId)
     ++ allDifferentOutputs(globalComponents, globalComponentsStmtId, globalComponentsExeId)
     ++ (if (componentsMultipleOccurrences) Nil else allDifferentInputs(globalComponents, globalComponentsStmtId, globalComponentsExeId))
     ++ componentSemanticsConstraints(globalComponents, testId, globalComponentsStmtId, globalComponentsExeId))

    val localAndGlobalDifferent = expressionComponents.map({
      case (v, bound, stmtId, exeId, components) =>
        differentOutputsSets(components, stmtId, exeId, globalComponents, globalComponentsStmtId, globalComponentsExeId)
    }).flatten

    val globalComponentLocality = 
      separators.map({
        case separator => globalComponents.map(componentLocality(_, globalComponentsStmtId, globalComponentsExeId, separator))
      }).flatten

    val additionalConstraints = globalComponentsConstraints ++ localAndGlobalDifferent ++ globalComponentLocality

    beginningConstraint :: endConstraint :: constraints ++ additionalConstraints
  }

  /**
    * Output and inputs are in the same expression range
    */
  def componentLocality(component: Component, stmtId: Int, exeId: Int, separator: SeparatorVariable): ComponentFormulaExpression = {
    def L(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(Location(ExecutionInstance(v, stmtId, exeId)))

    val functionComponents = filterComponentsByType[FunctionComponent](component :: Nil) //FIXME this could be simplified because we have only one component here
    val output = ComponentOutput(component)
    val inputs =
      functionComponents.map({
        case fc => fc.inputs.zipWithIndex.map({
          case (_, index) => ComponentInput(fc, index)
        })
      }).flatten

    val locality =
      (((Variable[ComponentVariable](separator) <= L(output)) & 
          inputs.foldLeft[ComponentFormulaExpression](true)({ 
            case (expr, input) => expr & (Variable[ComponentVariable](separator) <= L(input))
          }))
     | ((Variable[ComponentVariable](separator) > L(output)) &
          inputs.foldLeft[ComponentFormulaExpression](true)({ 
            case (expr, input) => expr & (Variable[ComponentVariable](separator) > L(input))
          })))

    locality
  }

  //FIXME these constraints should not depend on stmtId
  def differentOutputsSets(c1: List[Component], stmtId1: Int, exeId1: Int, c2: List[Component], stmtId2: Int, exeId2: Int): List[ComponentFormulaExpression] = {
    def L1(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(Location(ExecutionInstance(v, stmtId1, exeId1)))
    def L2(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(Location(ExecutionInstance(v, stmtId2, exeId2)))

    val outputs1 = c1.map(ComponentOutput)
    val outputs2 = c2.map(ComponentOutput)

    outputs1.map(v1 =>
          outputs2.map(v2 =>
            if (v1.comp.id == v2.comp.id) Nil
            else (!(L1(v1) === L2(v2))) :: Nil).flatten).flatten
  }

  def allDifferentOutputs(components: List[Component], stmtId: Int, exeId: Int): List[ComponentFormulaExpression] = {
    differentOutputsSets(components, stmtId, exeId, components, stmtId, exeId)
  }

  def allDifferentInputs(components: List[Component], stmtId: Int, exeId: Int): List[ComponentFormulaExpression] = {
    val functionComponents = filterComponentsByType[FunctionComponent](components)
    functionComponents.map(allDifferentInputsAux(_, stmtId, exeId)).flatten
  }

  def allDifferentInputsAux(component: FunctionComponent, stmtId: Int, exeId: Int): List[ComponentFormulaExpression] = {
    def L(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(Location(ExecutionInstance(v, stmtId, exeId)))

    val inps = collectInputs(component)

    def diffLoc(l: ComponentInput, ls: List[ComponentInput]): List[ComponentFormulaExpression] = 
      ls.map({ case v => !(L(v) === L(l)) }) ++ (ls match { case Nil => Nil case h :: t => diffLoc(h, t)})

    inps match {
      case h :: t => diffLoc(h, t)
      case _ => Nil
    }
  }


  /**
    * Semantics for functions and variables. Const components have stmt instance of output that is used to assign concrete values
    */
  def componentSemanticsConstraints(components: List[Component], 
                                    testId: Int, 
                                    stmtId: Int,
                                    exeId: Int): List[ComponentFormulaExpression] = {
    def local(v: ConnectionVariable): Variable[ComponentVariable] =
      //TODO What instance should be here?
      Variable(TestExecutionInstance(v, testId, stmtId, exeId))
    def tst(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(TestInstance(v, testId))
    def L(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(Location(ExecutionInstance(v, stmtId, exeId)))

    val functionComponents = filterComponentsByType[FunctionComponent](components)
    val variableComponents = filterComponentsByType[VariableComponent](components)

    val funcLib =
      functionComponents.map(comp => {
        def varMapping(v: ProgramVariable): Variable[ComponentVariable] = {
          val index = comp.inputs.indexOf(v)
          local(ComponentInput(comp, index))
        }
        def ufMapping(v: ProgramVariable): Variable[ComponentVariable] = {
          tst(BindingVariable(v))
        }
        val o = local(ComponentOutput(comp))
        comp.output match {
          case IntegerType() =>
            o === ProgramFormulaUtils.substitute[ComponentVariable](comp.semantics, varMapping, ufMapping)
          case BooleanType() =>
            o <=> ProgramFormulaUtils.substitute[ComponentVariable](comp.semantics, varMapping, ufMapping)
          case PointerType() => 
            if (Utils.verbose) println("[error] encoding: functions with pointer output are not supported")
            ???
        }
      })
    
    //test instances of BindingVariable refer exactly to the outputBinding of some of the
    // previous statements that assign corresponding variables:
    val varLib = variableComponents.map({
      case vc if vc.variable.typ == IntegerType() =>
        local(ComponentOutput(vc)) === tst(BindingVariable(vc.variable)) //tst because for different exeId we choose direrect binding variables
      case vc if vc.variable.typ == PointerType() =>
        local(ComponentOutput(vc)) === tst(BindingVariable(vc.variable))
      case vc if vc.variable.typ == BooleanType() =>
        local(ComponentOutput(vc)) <=> tst(BindingVariable(vc.variable))
    })

    funcLib ++ varLib
  }

  /**
    * Generates synthesis constraints for a set components allocated within a range with a output binding.
    * - range and acyclicity
    * - consistency (all different) + all different inputs if necessary
    * - lib
    * - conn (here we also consider global components)
    */
  def generateLocalConstraints(components: List[Component],
                               globalComponents: List[Component],
                               outputBinding: BindingVariable,
                               range: (SeparatorVariable, SeparatorVariable),
                               bound: Option[Int],
                               testId: Int,
                               stmtId: Int,
                               exeId: Int): List[ComponentFormulaExpression] = {
    // shortcuts:
    def local(v: ConnectionVariable): Variable[ComponentVariable] =
      //TODO What instance should be here?
      Variable(TestExecutionInstance(v, testId, stmtId, exeId))
    def tst(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(TestInstance(v, testId))
    def stmt(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(StatementInstance(v, stmtId))
    def L(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(Location(ExecutionInstance(v, stmtId, exeId)))

    //for global components
    def localG(v: ConnectionVariable): Variable[ComponentVariable] =
      //TODO What instance should be here?
      Variable(TestExecutionInstance(v, testId, globalComponentsStmtId, globalComponentsExeId))
    def tstG(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(TestInstance(v, testId))
    def stmtG(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(StatementInstance(v, globalComponentsStmtId))
    def LG(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(Location(ExecutionInstance(v, globalComponentsStmtId, globalComponentsExeId)))

    val (beg, end) = range

    // well-formedness
    val wfp =
      (((Variable[ComponentVariable](beg) <= L(outputBinding)) & 
        (L(outputBinding) < Variable[ComponentVariable](end)))
     :: inputsAndOutputsRange(components, range, stmtId, exeId)
     ++ allDifferentOutputs(components, stmtId, exeId)
     ++ (if (componentsMultipleOccurrences) Nil else allDifferentInputs(components, stmtId, exeId)))

    // semantics:
    val lib = componentSemanticsConstraints(components, testId, stmtId, exeId)

    //TODO remove dublication

    val functionComponents  = filterComponentsByType[FunctionComponent](components)
    val variableComponents  = filterComponentsByType[VariableComponent](components)
    val intConstComponents  = filterComponentsByType[IntegerConstantComponent](components)
    val boolConstComponents = filterComponentsByType[BooleanConstantComponent](components)

    val functionGlobalComponents  = filterComponentsByType[FunctionComponent](globalComponents)
    val variableGlobalComponents  = filterComponentsByType[VariableComponent](globalComponents)
    val intConstGlobalComponents  = filterComponentsByType[IntegerConstantComponent](globalComponents)
    val boolConstGlobalComponents = filterComponentsByType[BooleanConstantComponent](globalComponents)

    val groupedFunctionOutputs = functionComponents.groupBy({ case c => c.output }).mapValues(_.map(ComponentOutput))
    val groupedVariableOutputs = variableComponents.groupBy({ case c => c.variable.typ }).mapValues(_.map(ComponentOutput))
    val integerConstOutputs = intConstComponents.map(ComponentOutput)
    val booleanConstOutputs = boolConstComponents.map(ComponentOutput)

    val groupedGlobalFunctionOutputs =
      functionGlobalComponents.groupBy({ case c => c.output }).mapValues(_.map(ComponentOutput))
    val groupedGlobalVariableOutputs =
      variableGlobalComponents.groupBy({ case c => c.variable.typ }).mapValues(_.map(ComponentOutput))
    val integerGlobalConstOutputs = intConstGlobalComponents.map(ComponentOutput)
    val booleanGlobalConstOutputs = boolConstGlobalComponents.map(ComponentOutput)
       
    val outputs = groupedFunctionOutputs.map(_._2).flatten ++
                  groupedVariableOutputs.map(_._2).flatten ++
                  booleanConstOutputs ++ integerConstOutputs

    val outputsG = groupedGlobalFunctionOutputs.map(_._2).flatten ++
                   groupedGlobalVariableOutputs.map(_._2).flatten ++
                   booleanGlobalConstOutputs ++ integerGlobalConstOutputs

    val inputsAndTypes =
      functionComponents.map({
        case fc => fc.inputs.zipWithIndex.map({ case (v, index) => (ComponentInput(fc, index), v.typ) })
      }).flatten
    val groupedInputs = inputsAndTypes.groupBy(_._2).mapValues(_.map(_._1))

    val inputsAndTypesGlobal =
      functionGlobalComponents.map({
        case fc => fc.inputs.zipWithIndex.map({ case (v, index) => (ComponentInput(fc, index), v.typ) })
      }).flatten
    val groupedGlobalInputs = inputsAndTypesGlobal.groupBy(_._2).mapValues(_.map(_._1))

    def mergeGrouped[A,B](g1: Map[A, List[B]], g2: Map[A, List[B]]): Map[A, List[B]] = {
      (g1.toList ++ g2.toList).groupBy(_._1).mapValues(_.map(_._2).flatten)
    }

    // connecting semantics and location:
    val localGroupedOutputs = mergeGrouped(groupedFunctionOutputs, groupedVariableOutputs)
    val stmtGroupedOutputs = Map(IntegerType() -> integerConstOutputs,
                                 BooleanType() -> booleanConstOutputs)

    def unboxLs[T](lso: Option[List[T]]): List[T] = lso match { case None => Nil case Some(l) => l }

    val localGlobalGroupedOutputs = mergeGrouped(groupedGlobalFunctionOutputs, groupedGlobalVariableOutputs)
    val stmtGlobalGroupedOutputs = Map(IntegerType() -> integerGlobalConstOutputs,
                                       BooleanType() -> booleanGlobalConstOutputs)

    def variablesToLocationsMapping(localVars: List[ConnectionVariable],
                                    localVarsG: List[ConnectionVariable],
                                    stmtVars: List[ConnectionVariable],
                                    stmtVarsG: List[ConnectionVariable]):
                                     List[(Variable[ComponentVariable], Variable[ComponentVariable])] = {
         ((localVars.map(L)     ++ localVarsG.map(LG)     ++ stmtVars.map(L)    ++ stmtVarsG.map(LG))
      zip (localVars.map(local) ++ localVarsG.map(localG) ++ stmtVars.map(stmt) ++ stmtVarsG.map(stmtG)))
    }
    val variableLocationsForTypes = 
      supportedTypes.map({ case x => (x, x) }).toMap.mapValues({ case t =>
        variablesToLocationsMapping(unboxLs(groupedInputs.get(t)) ++ unboxLs(localGroupedOutputs.get(t)),
                                    unboxLs(groupedGlobalInputs.get(t)) ++ unboxLs(localGlobalGroupedOutputs.get(t)),
                                    unboxLs(stmtGroupedOutputs.get(t)),
                                    unboxLs(stmtGlobalGroupedOutputs.get(t))) ++
        (if (outputBinding.v.typ == t) List((L(outputBinding), tst(outputBinding))) else Nil)
      })

    //TODO (optimization) probably, we don't need to compare inputs with each other (same for outputs)
    val conn =
      variableLocationsForTypes.map({ case (t1, locations1) =>
        variableLocationsForTypes.map({ case (t2, locations2) =>
          locations1.map({ case (l1, v1) =>
            locations2.map({ case (l2, v2) =>
              (t1, t2) match {
                case (BooleanType(), BooleanType()) => (l1 === l2) -> (v1 <=> v2)
                case (x, y) if x == y => (l1 === l2) -> (v1 === v2)
                case _ => !(l1 === l2)
              }
            })
          }).flatten
        }).flatten
      }).flatten

    val boundConstraints: List[ComponentFormulaExpression] = bound match {
      case None => Nil
      case Some(value) => ((0 <= tst(outputBinding)) & (tst(outputBinding) < value)) :: Nil
    }

    wfp ++ lib ++ conn ++ boundConstraints
  }

  /**
    * Generates constraints for concrete expression using component representation of expression.
    *
    * @param presentation component presentation
    * @param outputBinding variable to bind the output of the expression
    * @param stmtId current test identifier
    * @return soft and hard constraints for concrete expression
    */
  def encodeStructure(presentation: ComponentPresentation, outputBinding: BindingVariable, stmtId: Int, exeId: Int):
      (List[ComponentFormulaExpression], List[ComponentFormulaExpression]) = {
    // shortcuts:
    def stmt(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(StatementInstance(v, stmtId))
    def L(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(Location(ExecutionInstance(v, stmtId, exeId)))

    def buildLocationConstraints(presentation: ComponentPresentation):
        (ConnectionVariable, (List[ComponentFormulaExpression], List[ComponentFormulaExpression])) = {
      presentation match {
        case FunctionComponentNode(component, alternatives, soft, hard) =>
          val (softOutputsBindings, innerConstraintsPairsSoft) = soft.keys.toList.map({
            case v =>
              val (o, ic) = buildLocationConstraints(soft(v))
              ((v, o), ic)
          }).unzip
          val (softOfSoftInputs, hardOfSoftInputs) = {
            val (s, h) = innerConstraintsPairsSoft.unzip
            (s.flatten, h.flatten)
          }
          val (hardOutputsBindings, innerConstraintsPairsHard) = hard.keys.toList.map({
            case v =>
              val (o, ic) = buildLocationConstraints(hard(v))
              ((v, o), ic)
          }).unzip
          val (softOfHardInputs, hardOfHardInputs) = {
            val (s, h) = innerConstraintsPairsHard.unzip
            (s.flatten, h.flatten)
          }
          val softCurrentConstraints =
            (component :: alternatives).map({
              case c: FunctionComponent =>
                softOutputsBindings.map({
                  case (v, out) => L(ComponentInput(c, c.inputs.indexOf(v))) === L(out)
                })
              case c: PhantomComponent =>
                softOutputsBindings.map({
                  case (v, out) => L(ComponentInput(c, c.inputs.indexOf(v))) === L(out)
                })
            }).flatten
          val hardCurrentConstraints =
            (component :: alternatives).map({
              case c: FunctionComponent =>
                hardOutputsBindings.map({
                  case (v, out) => L(ComponentInput(c, c.inputs.indexOf(v))) === L(out)
                })
              case c: PhantomComponent =>
                hardOutputsBindings.map({
                  case (v, out) => L(ComponentInput(c, c.inputs.indexOf(v))) === L(out)
                })
            }).flatten
          val totalSoft = softOfSoftInputs ++ softOfHardInputs ++ softCurrentConstraints
          val totalHard = hardOfSoftInputs ++ hardOfHardInputs ++ hardCurrentConstraints
          (ComponentOutput(component), (totalSoft, totalHard))
        case VariableComponentNode(component) =>
          (ComponentOutput(component), (Nil, Nil))
        case BooleanConstantComponentNode(component, value, isSoft) =>
          val constraints =
            if (isSoft) ((stmt(ComponentOutput(component)) === value) :: Nil, Nil)
            else (Nil, (stmt(ComponentOutput(component)) === value) :: Nil)
          (ComponentOutput(component), constraints)
        case IntegerConstantComponentNode(component, value, isSoft) =>
          val constraints =
            if (isSoft) ((stmt(ComponentOutput(component)) === value) :: Nil, Nil)
            else (Nil, (stmt(ComponentOutput(component)) === value) :: Nil)
          (ComponentOutput(component), constraints)
      }

    }

    val (output, (softConstraints, hardConstraints)) = buildLocationConstraints(presentation)

    ((L(output) === L(outputBinding)) :: softConstraints, hardConstraints)
  }

  def linkToPhantom(stmtId: Int, exeId: Int, binding: Binding, phantom: Binding): List[ComponentFormulaExpression] = {
    def stmt(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(StatementInstance(v, stmtId))
    def L(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(Location(ExecutionInstance(v, stmtId, exeId)))
    def LP(v: ConnectionVariable): Variable[ComponentVariable] =
      Variable(Location(ExecutionInstance(v, stmtId, StatementLevelRepair.phantomExeId)))

    val (phantomVar, phantomComps, phantomAdditional, phantomPres) = phantom
    val (v, comps, additional, pres) = binding

    def linkComponents(cmp: Component, cmpP: Component): List[ComponentFormulaExpression] = {
      cmpP match {
        case ph: PhantomComponent =>
          cmp match {
            case f: FunctionComponent =>
              ((L(ComponentOutput(f)) === LP(ComponentOutput(ph))) :: 
              f.inputs.zipWithIndex.map({ case (v, index) => (L(ComponentInput(f, index)) === LP(ComponentInput(ph, index))) }))
            case v: VariableComponent =>
              (L(ComponentOutput(v)) === LP(ComponentOutput(ph))) :: Nil
            case i: IntegerConstantComponent =>
              (L(ComponentOutput(i)) === LP(ComponentOutput(ph))) :: Nil
            case b: BooleanConstantComponent =>
              (L(ComponentOutput(b)) === LP(ComponentOutput(ph))) :: Nil
          }
      }
    }

    val additionalLinking = additional.zip(phantomAdditional).map({
      case (cmp, cmpP) => cmp match {
        case _: IntegerConstantComponent | _: BooleanConstantComponent =>
          (stmt(ComponentOutput(cmp)) === stmt(ComponentOutput(cmpP))) :: linkComponents(cmp, cmpP)
        case _ => linkComponents(cmp, cmpP)
      }
    }).flatten

    def linkPresentations(pres: ComponentPresentation, phantomPres: ComponentPresentation): List[ComponentFormulaExpression] = {
      (pres, phantomPres) match {
        case (FunctionComponentNode(cmp, alts, soft, hard), FunctionComponentNode(cmpP, altsP, softP, hardP)) =>
          linkComponents(cmp, cmpP) ++
          alts.zip(altsP).map({ case (c, cp) => linkComponents(c, cp) }).flatten ++
          (soft.values ++ hard.values).zip(softP.values ++ hardP.values).map({ case (p, pp) => linkPresentations(p, pp)}).flatten
        case (VariableComponentNode(cmp), FunctionComponentNode(cmpP, _, _, _)) =>
          linkComponents(cmp, cmpP)
        case (BooleanConstantComponentNode(cmp, value, isSoft), FunctionComponentNode(cmpP, _, _, _)) =>
          linkComponents(cmp, cmpP)
        case (IntegerConstantComponentNode(cmp, value, isSoft), FunctionComponentNode(cmpP, _, _, _)) =>
          linkComponents(cmp, cmpP)
      }
    }

    val linkingConstraints = linkPresentations(pres, phantomPres)

    (L(v) === LP(phantomVar)) :: (additionalLinking ++ linkingConstraints)
  }

}
