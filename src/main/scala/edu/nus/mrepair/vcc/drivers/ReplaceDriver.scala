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

object ReplaceDriver extends AbstractDriver {

  def benchmarkName = "replace"
  
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
      case "1"  => "dodash"
      case "2"  => "dodash"
      case "3"  => "subline"
      case "4"  => "subline"
      case "5"  => "dodash"
      case "7"  => "in_set_2_test"
      case "8"  => "in_set_2_test"
      case "10" => "dodash"
      case "11" => "dodash"
      case "16" => "in_set_2_test"
      case "25" => "test_omatch"
      case "26" => "test_omatch"
      case "27" => "in_pat_set_test"
      case "28" => "in_set_2_test"
      case "29" => "in_set_2_test"
      case "30" => "in_set_2_test"
    }
  }

  def suspiciousLocationsByVersion(version: String): SuspiciousLocations = {
    version match {
      case "1"  => SuspiciousLocations(List("dodash"), globalVariables = false)
      case "2"  => SuspiciousLocations(List("dodash"), globalVariables = false)
      case "3"  => SuspiciousLocations(List("subline"), globalVariables = false)
      case "4"  => SuspiciousLocations(List("subline"), globalVariables = false)
      case "5"  => SuspiciousLocations(List("dodash"), globalVariables = false)
      case "7"  => SuspiciousLocations(List("in_set_2"), globalVariables = false)
      case "8"  => SuspiciousLocations(List("in_set_2"), globalVariables = false)
      case "10" => SuspiciousLocations(List("dodash"), globalVariables = false)
      case "11" => SuspiciousLocations(List("dodash"), globalVariables = false)
      case "16" => SuspiciousLocations(List("in_set_2"), globalVariables = false)
      case "25" => SuspiciousLocations(List("omatch"), globalVariables = false)
      case "26" => SuspiciousLocations(List("omatch"), globalVariables = false)
      case "27" => SuspiciousLocations(List("in_pat_set"), globalVariables = false)
      case "28" => SuspiciousLocations(List("in_set_2"), globalVariables = false)
      case "29" => SuspiciousLocations(List("in_set_2"), globalVariables = false)
      case "30" => SuspiciousLocations(List("in_set_2"), globalVariables = false)
    }
  }

  def selectTestsByVersion(version: String): List[List[TestData]] = {
    loadTestData(version, 0) :: Nil
  }

}
