package edu.nus.mrepair.vcc.drivers

import scala.util.parsing.combinator._
import edu.nus.mrepair.synthesis.ProgramFormula
import ProgramFormula._
import ProgramFormula.ConstantConversion._
import java.io.File
import edu.nus.mrepair._
import edu.nus.mrepair.vcc._
import scala.collection.JavaConverters._
import edu.nus.mrepair.Utils
import edu.nus.mrepair.synthesis.{EncodingConfig, Linear}
import edu.nus.mrepair.SuspiciousLocations
import edu.nus.mrepair.synthesis.EncodingConfig
import edu.nus.mrepair.VCCBenchmark
import edu.nus.mrepair.SynthesisAST
import edu.nus.vctrans.ast.DefineFunAst
import edu.nus.vctrans.ast.DeclareFunAst
import edu.nus.mrepair.SMTParser
import org.smtlib.command.{C_assert, C_define_fun, C_declare_fun}
import org.smtlib.IExpr
import scala.io.Source

trait AbstractDriver {

  def benchmarkName: String

  def run(benchmarkVersion: String, synthesisConfig: SynthesisConfig, localization: Boolean): BenchmarkExecutionResult

  def loadSemfixTests(benchmarkVersion: String): List[Int] = {
    val source = Source.fromFile(f"benchmarks/$benchmarkName/$benchmarkVersion/semfix-tests")
    source.getLines().toList.map(Integer.parseInt)
  }

  def vcFile(benchmarkVersion: String): String = 
    f"benchmarks/$benchmarkName/$benchmarkVersion/vc.smt2"

  def loadGlobalDecls(benchmarkVersion: String): List[C_assert] = {
    val script = SMTParser.parseFile(new File(f"benchmarks/$benchmarkName/$benchmarkVersion/global-decls.smt2"))
    script.commands().asScala.toList.map({
      case asrt: C_assert => asrt
    })
  }

  def loadBackgroundPredicates(benchmarkVersion: String): List[BackgroundPredicate] = {
    val script = SMTParser.parseFile(new File(f"benchmarks/$benchmarkName/$benchmarkVersion/assertions.smt2"))
    script.commands().asScala.toList.map({
      case asrt: C_assert => BackgroundAssertion(asrt)
      case asrt: C_define_fun => BackgroundDefinition(new DefineFunAst(asrt))
      case asrt: C_declare_fun => 
        if (Utils.verbose) println("[error] declarations are not supported in background predicates")
        ???
        // disable because of difficulties with type lookup
        //BackgroundDeclaration(new DeclareFunAst(asrt))
    })
  }

  def loadTestData(benchmarkVersion: String, testId: Int): List[SMTlibAST] = {
    val file = new File(f"benchmarks/$benchmarkName/$benchmarkVersion/test$testId.smt2")
    val script = SMTParser.parseFile(file)
    script.commands().asScala.toList.map({ 
      case asrt: C_assert => SMTlibAST(asrt.expr()) 
    })
  }

}
