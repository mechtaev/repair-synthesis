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

object Schedule2Driver extends AbstractDriver {

  def benchmarkName = "schedule2"
  
  def run(benchmarkVersion: String, synthesisConfig: SynthesisConfig, localization: Boolean): BenchmarkExecutionResult = {
    if (localization && Utils.verbose) println("[warn] schedule does not support localization")
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
      case "1" => ???
      case "2" => ???
      case "3" => ???
      case "4" => ???
      case "5" => "new_job"
      case "6" => "get_command"
      case "7" => "get_process"
      case "8" => ???
      case "9" => ???
    }
  }

  def suspiciousLocationsByVersion(version: String): SuspiciousLocations = {
    version match {
      case "1" => ???
      case "2" => ???
      case "3" => ???
      case "4" => ???
      case "5" => SuspiciousLocations(List("new_job"), globalVariables = false)
      case "6" => SuspiciousLocations(List("get_command"), globalVariables = false)
      case "7" => SuspiciousLocations(List("get_process"), globalVariables = false)
      case "8" => ???
      case "9" => ???
    }
  }
  
  def selectTestsByVersion(version: String): List[List[TestData]] = {
    version match {
      case "1" => ???
      case "2" => ???
      case "3" => ???
      case "4" => ???
      case "5" => loadTestData(version, 0) :: Nil
      case "6" => loadTestData(version, 0) :: Nil
      case "7" => loadTestData(version, 0) :: Nil
      case "8" => ???
      case "9" => ???
    }
  }

}
