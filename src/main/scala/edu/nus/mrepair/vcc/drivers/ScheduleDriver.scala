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

object ScheduleDriver extends AbstractDriver {

  def benchmarkName = "schedule"
  
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
      case "1" => "find_nth"
      case "2" => "unblock_process"
      case "3" => "upgrade_process_prio"
      case "4" => "upgrade_process_prio"
      case "5" => ???
      case "6" => "find_nth"
      case "7" => ???
      case "8" => ???
      case "9" => "main"
    }
  }

  def suspiciousLocationsByVersion(version: String): SuspiciousLocations = {
    version match {
      case "1" => SuspiciousLocations(List("find_nth"), globalVariables = false)
      case "2" => SuspiciousLocations(List("unblock_process"), globalVariables = false)
      case "3" => SuspiciousLocations(List("upgrade_process_prio"), globalVariables = false)
      case "4" => SuspiciousLocations(List("upgrade_process_prio"), globalVariables = false)
      case "5" => ???
      case "6" => SuspiciousLocations(List("find_nth"), globalVariables = false)
      case "7" => ???
      case "8" => ???
      case "9" => SuspiciousLocations(List("main"), globalVariables = false)
    }
  }
  
  def selectTestsByVersion(version: String): List[List[TestData]] = {
    version match {
      case "1" => loadTestData(version, 0) :: loadTestData(version, 1) :: Nil
      case "2" => loadTestData(version, 0) :: Nil
      case "3" => ???
      case "4" => loadTestData(version, 0) :: Nil
      case "5" => ???
      case "6" => loadTestData(version, 0) :: loadTestData(version, 1) :: Nil
      case "7" => ???
      case "8" => ???
      case "9" => loadTestData(version, 0) :: Nil
    }
  }

}
