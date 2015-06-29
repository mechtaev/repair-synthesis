package edu.nus.mrepair.klee

import edu.nus.mrepair._
import edu.nus.mrepair.AngelicFix._
import edu.nus.mrepair.Utils._
import java.io.File
import edu.nus.mrepair.Utils.SimpleLogger._
import org.smtlib.IExpr
import scala.util.parsing.combinator._
import com.microsoft.z3._
import edu.nus.maxsmtplay._
import scala.collection.JavaConverters._
import edu.nus.mrepair.synthesis.{ProgramFormula, Formula, ComponentFormula}
import Formula._
import ProgramFormula._
import org.smtlib.command.C_assert
import edu.nus.mrepair.vcc.VCCUtils


/**
  * AngelicFix implementation top class
  */
object AFRepair {

  def debugMode(): Boolean = {
    sys.env.get("AF_DEBUG") match {
      case None => false
      case _ => true
    }
  }


  def generatePatch(synthesisConfig: SynthesisConfig,
                    extractedDir: String,
                    angelicForest: AngelicForest): Either[List[(Int, ProgramFormulaExpression, ProgramFormulaExpression)], Boolean] = {
    //TODO I need to be more clear from where I take ids: files or environment variable
    val suspiciousIds = (new File(extractedDir)).listFiles.filter(f => """.*\.smt2$""".r.findFirstIn(f.getName).isDefined).map(_.getName.dropRight(".smt2".length).toInt).toList
    val suspicious = suspiciousIds.map({
      case id =>
        val script = SMTParser.parseFile(new File(extractedDir + "/" + id + ".smt2"))
        val expr = script.commands().asScala.toList.map({
          case asrt: C_assert => asrt.expr()
        }).apply(0)
        (id, expr)
    })

    val (rc, oldExpr, components) = edu.nus.mrepair.klee.RCGenerator.generate(angelicForest, suspicious, synthesisConfig)

    val (patch, stat) = edu.nus.mrepair.klee.RCGenerator.solve(rc, components, oldExpr, synthesisConfig)

    patch
  }

  def getAvailableSymbols(formula: String, solver: MaxSMT with Z3): List[String] = {
    val origClauses = solver.z3.parseSMTLIB2String(formula + "\n(check-sat)\n(exit)", Array(), Array(), Array(), Array())
    solver.solveAndGetModel(Nil, origClauses :: Nil) match {
      case Some((_, origModel)) =>
        origModel.getConstDecls().toList.map({ case d => d.getName().toString})
      case None =>
        println("UNSAT")
        Nil
    }
  }

  /*
   Name format example:

   int!suspicious!0 = 3
   int!suspicious!0!env!x = 1
   int!input!y = 3
   int!output!stdout = 4
   int!suspicious!0!env!y = 3
   int!input!x = 1
   int!suspicious!0!original = 4
  */
  def getAngelicPath(assignment: List[VariableValue]): AngelicPath = {
    val suspiciousPrefix = "suspicious!"
    assignment.toList.filter({
      case IntVal(n, v)  => n.startsWith(suspiciousPrefix)
      case BoolVal(n, v) => n.startsWith(suspiciousPrefix)
      case CharVal(n, v) => n.startsWith(suspiciousPrefix)
    }).map({
      case IntVal(n, v) =>
        val id :: rest = n.drop(suspiciousPrefix.length).split("!").toList
        (id.toInt, (rest, IntVal(n, v)))
      case BoolVal(n, v) =>
        val id :: rest = n.drop(suspiciousPrefix.length).split("!").toList
        (id.toInt, (rest, BoolVal(n, v)))
      case CharVal(n, v) =>
        val id :: rest = n.drop(suspiciousPrefix.length).split("!").toList
        (id.toInt, (rest, CharVal(n, v)))
    }).groupBy(_._1).toList.map({
      case (id, assignment) =>
        var original: Option[VariableValue] = None
        var angelic: Option[VariableValue] = None
        val context = assignment.foldLeft(List[VariableValue]())({
          case (acc, (_, ("original" :: Nil, v))) =>
            original = Some(v)
            acc
          case (acc, (_, ("angelic" :: Nil, v))) =>
            angelic = Some(renameVal(v, "suspicious" + id))
            acc
          case (acc, (_, ("env" :: name :: Nil, v))) =>
            renameVal(v, name) :: acc
        })
        AngelicValue(context, angelic.get, id)
    })
  }

  /**
    * Solving smt files generated by KLEE for each test case
    */
  def generateAngelicForest(smtFiles: List[String], testUniverseDir: String, testSuiteIds: List[String]): AngelicForest = {
    val af = scala.collection.mutable.Map[String, List[AngelicPath]]()
    testSuiteIds.foreach({ case id => af += id -> Nil })

    val solver = new Sat() with Z3
    solver.init(None)

    var repairedTests: List[String] = Nil

    smtFiles.map({
      case file =>
        if (debugMode()) println("[synthesis] checking path " + file)
        val formula = scala.io.Source.fromFile(file).mkString
        val afVars = getAvailableSymbols(formula, solver).filter({case s => s.startsWith("int!") || s.startsWith("bool!") || s.startsWith("char!")})
        solver.solver.reset()

        if (! afVars.isEmpty) {

          val getter = afVars.map({
            case varName =>
              s"(declare-fun aux_$varName () (_ BitVec 32)) (assert (= aux_$varName (concat (select $varName (_ bv3 32)) (concat (select $varName (_ bv2 32)) (concat (select $varName (_ bv1 32)) (select $varName (_ bv0 32)))))))"
          }).mkString(" ")

          testSuiteIds.foreach({
            case testId =>

              if (debugMode()) println("[synthesis] checking test " + testId)
              val (in, out) = getTestData(testUniverseDir, testId)

              //TODO: refactor this long strings:
              val inputAssertion = in.map({
                case IntVal(name, value) =>
                  val varName = "int!input!" + name
                  if(afVars.contains(varName)) {
                    s"(assert (= ((_ int2bv 32) $value) (concat (select $varName (_ bv3 32)) (concat (select $varName (_ bv2 32)) (concat (select $varName (_ bv1 32)) (select $varName (_ bv0 32)))))))"
                  } else {
                    ""
                  }
                case BoolVal(name, value) =>
                  val varName = "bool!input!" + name
                  if(afVars.contains(varName)) {
                    if (value) {
                      s"(assert (not (= (_ bv0 32) (concat (select $varName (_ bv3 32)) (concat (select $varName (_ bv2 32)) (concat (select $varName (_ bv1 32)) (select $varName (_ bv0 32))))))))"
                    } else {
                      s"(assert (= (_ bv0 32) (concat (select $varName (_ bv3 32)) (concat (select $varName (_ bv2 32)) (concat (select $varName (_ bv1 32)) (select $varName (_ bv0 32)))))))"
                    }
                  } else {
                    ""
                  }
                case CharVal(name, value) =>
                  val varName = "char!input!" + name
                  val valAsInt = value.asInstanceOf[Int] // basically, ord
                  if(afVars.contains(varName)) {
                    s"(assert (= (_ bv$valAsInt 8) (select $varName (_ bv0 32))))"
                  } else {
                    ""
                  }
              }).mkString("\n")

              var unconstraintOutputVars: List[String] = Nil;

              val outputAssertion = out.map({
                case IntVal(name, value) =>
                  val varName = "int!output!" + name
                  if(afVars.contains(varName)) {
                    s"(assert (= ((_ int2bv 32) $value) (concat (select $varName (_ bv3 32)) (concat (select $varName (_ bv2 32)) (concat (select $varName (_ bv1 32)) (select $varName (_ bv0 32)))))))"
                  } else {
                    unconstraintOutputVars = name :: unconstraintOutputVars;
                    ""
                  }
                case BoolVal(name, value) =>
                  val varName = "bool!output!" + name
                  if(afVars.contains(varName)) {
                    if (value) {
                      s"(assert (not (= (_ bv0 32) (concat (select $varName (_ bv3 32)) (concat (select $varName (_ bv2 32)) (concat (select $varName (_ bv1 32)) (select $varName (_ bv0 32))))))))"
                    } else {
                      s"(assert (= (_ bv0 32) (concat (select $varName (_ bv3 32)) (concat (select $varName (_ bv2 32)) (concat (select $varName (_ bv1 32)) (select $varName (_ bv0 32)))))))"
                    }
                  } else {
                    unconstraintOutputVars = name :: unconstraintOutputVars;
                    ""
                  }
                case CharVal(name, value) =>
                  val varName = "char!output!" + name
                  val valAsInt = value.asInstanceOf[Int] // basically, ord
                  if(afVars.contains(varName)) {
                    s"(assert (= (_ bv$valAsInt 8) (select $varName (_ bv0 32))))"
                  } else {
                    unconstraintOutputVars = name :: unconstraintOutputVars;
                    ""
                  }
              }).mkString("\n")

              if (!unconstraintOutputVars.isEmpty) {
                if (debugMode()) {
                  println("[synthesis] unconstraint output variables: " + unconstraintOutputVars.mkString(" "))
                }
              } else {
                val ending = "\n(check-sat)\n(exit)"
                val clauses = solver.z3.parseSMTLIB2String(formula + inputAssertion + outputAssertion + getter + ending, Array(), Array(), Array(), Array())
                solver.solveAndGetModel(Nil, clauses :: Nil) match {
                  case Some((_, model)) =>
                    repairedTests = testId :: repairedTests
                    val result = afVars.map({
                      case varName =>
                        val auxName = "aux_" + varName
                        val raw_value = model.eval(solver.z3.mkBVConst(auxName, 32), false)
                        val value =
                          try {
                            raw_value.asInstanceOf[BitVecNum].getInt
                          } catch {
                            case _ =>
                              val v = raw_value.toString.toLong - 4294967296L
                              if (debugMode()) println("[synthesis] evaluating " + varName + " value " + raw_value + " as " + v)
                              v
                          }
                        (varName, value)
                    })
                    if (debugMode()) result.foreach({ case (n, v) => println("[synthesis] " + n + " = " + v)})
                    //TODO temporary
                    val ap = getAngelicPath(result.foldLeft(List[VariableValue]())({
                      case (acc, (n, v)) =>
                        if (n.startsWith("int!suspicious!")) {
                          IntVal(n.drop("int!".length), v.toInt) :: acc
                        } else if (n.startsWith("bool!suspicious!")) {
                          BoolVal(n.drop("bool!".length), v != 0L) :: acc
                        } else {
                          acc
                        }
                    }))
                    af(testId) = ap :: af(testId)
                  case None => if (debugMode()) println("[synthesis] UNSAT")
                }
              }
              solver.solver.reset()
          })
        }
    })
    solver.delete()

    println("[synthesis] Paths explored: " + smtFiles.length)
    println("[synthesis] Angelic values generated: " + repairedTests.length)
    println("[synthesis] Test cases covered: " + repairedTests.distinct.length + "/" + testSuiteIds.length)

    af.toList.toMap
  }


  def getTestData(testUniverseDir: String, testId: String): (List[VariableValue], List[VariableValue]) = {
    val inputsFile = testUniverseDir + File.separator + testId + ".in"
    val outputsFile = testUniverseDir + File.separator + testId + ".out"
    val inputsSource = scala.io.Source.fromFile(inputsFile)
    val outputsSource = scala.io.Source.fromFile(outputsFile)
    val inputsContent = inputsSource.mkString
    val outputsContent = outputsSource.mkString
    val in = TestMappingParser(inputsContent)
    val out = TestMappingParser(outputsContent)
    inputsSource.close()
    outputsSource.close()
    (in, out)
  }


  object TestMappingParser extends JavaTokenParsers {
    def apply(input: String): List[VariableValue] = parseAll(inputs, input).get
    def inputs = rep(input)
    def input =
      ( ident ~ "=" ~ "'" ~ ident ~ "'" ^^ { case n ~ "=" ~ "'" ~ c ~ "'" => CharVal(n, c.charAt(0)) }
      | ident ~ "=true" ^^ { case n ~ "=true" => BoolVal(n, true) }
      | ident ~ "=false" ^^ { case n ~ "=false" => BoolVal(n, false) }
      | ident ~ "=" ~ wholeNumber ^^ { case n ~ "=" ~ si => IntVal(n, Integer.parseInt(si)) })
  }

}
