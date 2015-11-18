package edu.nus.mrepair.vcc

import edu.nus.mrepair._
import edu.nus.mrepair.synthesis.ProgramFormula._
import edu.nus.mrepair.StatementLevelRepair._
import edu.nus.mrepair.decision._
import edu.nus.vctrans.ast._
import edu.nus.mrepair.Utils.SimpleLogger._
import edu.nus.mrepair.Utils._
import scala.collection.JavaConverters._
import org.smtlib.IExpr
import edu.nus.mrepair.synthesis.ComponentDecoder
import edu.nus.mrepair.synthesis.ComponentFormula.Component
import edu.nus.mrepair.vcc.translation.TranslationCommon._
import edu.nus.mrepair.StatementLevelRepair.RepairableBindings
import edu.nus.mrepair.vcc.VCCUtils.TFRepairableExpressions
import org.smtlib.command.{C_assert, C_define_fun}

object RCGenerator {


  def generate(tf: TF,
               repairable: TFRepairableExpressions,
               entryFunction: String,
               tests: List[List[TestData]],
               suspicious: SuspiciousLocations,
               assertions: List[BackgroundPredicate],
               repairConfig: SynthesisConfig): (RepairCondition, List[Component]) = {

    val declsIndex = VCCUtils.buildDeclsIndex(tf.declarations)

    val isSuspiciousFunction = suspicious match {
      case SuspiciousLocations(functions, _) => (f: String) => functions.contains(f)
    }

    val Some(entryProcedure) = tf.procedures.find({
      case p: Procedure => p.name == entryFunction
    })

    val suspiciousProcedures = tf.procedures.filter({
      case Procedure(name, _, _, _) => isSuspiciousFunction(name)
    })

    //FIXME here we do it flat temporary
    val repairableBindings = suspiciousProcedures.map({
      case Procedure(name, _, _, _) => repairable.byFunction(name)
    }).flatten ++ repairable.inGlobalDecls.flatten

    val (repairableObjects, extractedComponents) = 
      extractRepairableObjects(repairableBindings, repairConfig.synthesisConfig, repairConfig.componentLevel)

    val sharedComponents = getSharedComponents(repairConfig.componentLevel)
    
    val (softStructureExpr, hardStructureExpr) = generateStructureConstraints(repairableObjects, repairConfig.synthesisConfig)

    val (softStructure, hardStructure) = (softStructureExpr.map(FormulaAST), hardStructureExpr.map(FormulaAST))

    val semanticsConstraints = tests.zipWithIndex.map({
      case (testExpressions, testId) =>
        val subsVars = repairableBindings.map(_._1)
        val subsVarsDecls = subsVars.map(VCCUtils.getDeclBuilder(testId))
        val subsVarsDeclsString = subsVarsDecls.foldLeft("")(_ + "\n" + _)
        val (allDefinesLists, orderLists) = tf.procedures.map({
          case Procedure(_, _, instances, o) => (instances, o)
        }).unzip
        val allDefines = allDefinesLists.flatten
        val order = orderLists.flatten

        val renameDefine = (d: DefineFunAst) => {
          def defRenamer: (String => String) = (name: String) => {
            if (d.getParameters.asScala.toList.contains(name)) {
              "|" + name + "|"
            } else {
              val renameBuggy = VCCUtils.getSymbolRenamer(testId, tf, declsIndex, repairableBindings)
              renameBuggy(name)
            }
          }
          val renameBody = (body: IExpr) => VCCUtils.renameExpr(body, defRenamer)
          val renameFunc = VCCUtils.genericRenamer(testId)
          val renamed = d.rename(renameFunc, renameBody).toString
          renamed
        }

        val allDefinesRenamed = allDefines.map(renameDefine)
        val allDefinesStrings = prettyList(allDefinesRenamed.zip(order).sortBy(_._2).map(_._1))
        val entryRepairable = repairableBindings //TODO if it is correct, remove the old version:
          //if (repairable.contains(entryFunction)) repairable(entryFunction)
          //else List[(ProgramVariable, ProgramFormulaExpression)]()
        val mainRenamer = VCCUtils.getSymbolRenamer(testId, tf, declsIndex, entryRepairable)
        val funAst = entryProcedure.assertion
        if (Utils.enableLogging) funAst.toString.log("rc-entry-function-ast.log")
        val mainBody = funAst.getExpr
        val mainBodyString = "\n(assert " + VCCUtils.renameExpr(mainBody, mainRenamer).accept(new PrettyPrinter()) + ")"
        val allSortsStrings = prettyList(tf.sorts)
        val allDeclsStrings =
          prettyList(tf.declarations.map({
            case d: DeclareFunAst => d.rename(VCCUtils.getVariableRenamerByType(testId, VCCUtils.typeStringToAst(d.getType))).toString
          }))
        //here is a bit ugly: we remove assert ast and add assert string:
        val globalDeclsStrings = 
          prettyList(tf.globalDecls.map((assert: C_assert) => VCCUtils.renameExpr(assert.expr(), mainRenamer)).map({ case s => "\n(assert " + s + ")" }))
        if (Utils.enableLogging) globalDeclsStrings.log("tc-globalDecls")
        
        val backgroundDefinitions = assertions.filter({ case a: BackgroundDefinition => true case _ => false}).asInstanceOf[List[BackgroundDefinition]]
        //specially for test data:
        val backgroundDefinitionsString = prettyList(backgroundDefinitions.map({ case bd => renameDefine(bd.definition).toString }))

        val assertionsStrings = {
          val renamed = assertions.map({
            case BackgroundDefinition(definition) =>
              renameDefine(definition).toString
            case BackgroundAssertion(assertion) => 
              val a = VCCUtils.renameExpr(assertion.expr(), mainRenamer).toString
              "(assert " + a + ")"
          })
          prettyList(renamed)
        }
        if (Utils.enableLogging) assertionsStrings.log("tc-assertions")

        //TODO do we actually need foralls?
//        val foralls =
//          funAst.getAssumptions.asScala.toList.map(allDeclsStrings + "\n(assert " + _.toString + ")").map(FormulaString)
        //TODO on the other hand, it makes sense to add assertions for arrays to resulting formula (tf.asserts)

        val semanticsPart =
          FormulaString(allSortsStrings + allDeclsStrings + subsVarsDeclsString + allDefinesStrings + globalDeclsStrings + assertionsStrings + mainBodyString)

        val testsPart = testExpressions.map({
          case SynthesisAST(formula) => FormulaAST(testConstraintsForTestCase(formula, testId))
          case SMTlibAST(formula) =>
            val str = allSortsStrings + allDeclsStrings + backgroundDefinitionsString + "\n" + "\n(assert " + VCCUtils.renameExpr(formula, mainRenamer).toString + ")"
            FormulaString(str)
        })

        val synthesisPart = semanticsConstraintsForTestCase(repairableObjects, sharedComponents, testId, repairConfig.synthesisConfig).map(FormulaAST)

        //foralls ++
        semanticsPart :: (testsPart ++ synthesisPart)
    }).flatten
    val components = extractedComponents ++ sharedComponents
    if (Utils.enableLogging) prettyList(components).log("rc-components.log")

    (RepairCondition(hardStructure ++ semanticsConstraints, softStructure), components)
  }

  /**
    * Solve RC and construct patch
    */
  def solve(rc: RepairCondition,
            repairable: TFRepairableExpressions,
            components: List[Component],
            repairConfig: SynthesisConfig): (Either[List[(ProgramFormulaExpression, ProgramFormulaExpression)], Boolean], SolverStat) = {

    val RepairCondition(hardClauses, softClauses) = rc

    val solverResult =
      MaxSMTPlay.solve(hardClauses, softClauses, components, repairConfig.simplification, repairConfig.reuseStructure, repairConfig.solverBound, repairConfig.solverTimeout)

    // constructing patch:
    solverResult match {
      case Right(isTimeout) =>
        (Right(isTimeout), MaxSMTPlay.lastStat)
      case Left((numRemovedConstr, model)) =>
        if (Utils.enableLogging) prettyList(model).log("rc-model.log")
        
        val newAssignments = ComponentDecoder.decode(model)
        if (Utils.enableLogging) prettyList(newAssignments).log("rc-allfixed.log")
 
        // find changes:
        val joined = newAssignments.map({
          case (v, e) =>
            //TODO here we do it flat temporary
            val assignments = repairable.byFunction.toList.map(_._2).flatten ++ repairable.inGlobalDecls.flatten
            val Some((_, old, _, _, _)) = assignments.find({ case (vv, _, _, _, _) => vv == v }) //TODO here we can perform localization using cmp-id
            (old, e)
        })
        val patch = joined.filter({ case (old, fixed) => old != fixed })
        
        (Left(patch), MaxSMTPlay.lastStat)
    }
  }

}
