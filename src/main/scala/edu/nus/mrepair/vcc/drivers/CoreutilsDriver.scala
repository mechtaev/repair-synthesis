package edu.nus.mrepair.vcc.drivers

import edu.nus.mrepair.BenchmarkExecutionResult
import edu.nus.mrepair.synthesis.ProgramFormula._
import edu.nus.mrepair.synthesis.ProgramFormula.ConstantConversion._
import edu.nus.mrepair.VCCBenchmark
import edu.nus.mrepair.synthesis.EncodingConfig
import edu.nus.mrepair.SynthesisConfig
import edu.nus.mrepair.synthesis.Linear
import edu.nus.mrepair.vcc.VCCRepair
import edu.nus.mrepair.SuspiciousLocations
import edu.nus.mrepair.TestData
import edu.nus.mrepair.SynthesisAST
import edu.nus.mrepair.SMTParser
import edu.nus.mrepair.SMTlibAST
import edu.nus.mrepair.Utils
import org.smtlib.IExpr
import java.io.File
import scala.collection.JavaConverters._
import org.smtlib.command.{C_assert, C_define_fun}

object CoreutilsDriver extends AbstractDriver {

  def benchmarkName = "coreutils"
  
  def run(benchmarkVersion: String, synthesisConfig: SynthesisConfig, localization: Boolean): BenchmarkExecutionResult = {
    if (localization && Utils.verbose) println("[warn] coreutils do not support localization")
    val benchmark = VCCBenchmark(name          = benchmarkName,
                                 version       = benchmarkVersion,
                                 entryFunction = entryFunctionByVersion(benchmarkVersion),
                                 smtFile       = vcFile(benchmarkVersion),
                                 tests         = selectTestsByVersion(benchmarkVersion),
                                 localization  = false,
                                 suspicious    = suspiciousLocationsByVersion(benchmarkVersion),
                                 globalDecls   = Nil,
                                 assertions    = loadBackgroundPredicates(benchmarkVersion))

    VCCRepair.run(benchmark, synthesisConfig)
  }

  def entryFunctionByVersion(version: String): String = {
    version match {
      case "b58a8b4e" => "collapse_escapes"
      case "a0851554" => "copy_unescaped_string"
      case "7cb24684" => "bsd_split_3"
      case "72d05289" => "main"
      case "6856089f" => "char_to_clump"
    }
  }

  def suspiciousLocationsByVersion(version: String): SuspiciousLocations = {
    version match {
      case "b58a8b4e" => SuspiciousLocations(List("collapse_escapes"), globalVariables = false)
      case "7cb24684" => SuspiciousLocations(List("bsd_split_3"), globalVariables = false)
      case "72d05289" => SuspiciousLocations(List("main"), globalVariables = false)
      case "a0851554" => SuspiciousLocations(List("copy_unescaped_string"), globalVariables = false)
      case "6856089f" => SuspiciousLocations(List("char_to_clump", "isprint"), globalVariables = false)
    }
  }

  
  def selectTestsByVersion(version: String): List[List[TestData]] = {
    version match {
      case "7cb24684" => loadTestData(version, 0) :: /*loadTestData(version, 1) :: loadTestData(version, 2) :: loadTestData(version, 3) ::*/ Nil
      case _ => loadTestData(version, 0) :: Nil
    }
  }

}
