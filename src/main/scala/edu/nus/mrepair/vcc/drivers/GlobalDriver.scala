package edu.nus.mrepair.vcc.drivers

import edu.nus.mrepair.synthesis.ProgramFormula
import ProgramFormula._
import ProgramFormula.ConstantConversion._
import edu.nus.mrepair.Utils
import java.io.File
import edu.nus.mrepair.{SynthesisAST, SuspiciousLocations, VCCBenchmark}
import edu.nus.mrepair.synthesis.{EncodingConfig, Linear}
import edu.nus.mrepair.SynthesisConfig
import edu.nus.mrepair.BenchmarkExecutionResult
import edu.nus.mrepair.vcc.VCCRepair

/**
 * Created by seryozha on 4/14/14.
 */
object GlobalDriver extends AbstractDriver {

  def benchmarkName = "global"

  def run(benchmarkVersion: String, synthesisConfig: SynthesisConfig, localization: Boolean): BenchmarkExecutionResult = {
    if (localization && Utils.verbose) println("[warn] global does not support localization")
    val benchmark =
      VCCBenchmark(
        name          = benchmarkName,
        version       = benchmarkVersion,                         
        entryFunction = "global_test",
        smtFile       = "Subjects" + File.separator + "Simple" + File.separator + "global" + File.separator + "global_test.smt2",
        tests =
          ( (SynthesisAST((ivar("Global#input_i@$s") === 1) & (ivar("L#output_o@0") === 24)) :: Nil)
         :: (SynthesisAST((ivar("Global#input_i@$s") === -1) & (ivar("L#output_o@0") === 0)) :: Nil)
         :: Nil),
        localization  = false,
        suspicious    = SuspiciousLocations(List("global_test"), globalVariables = false),
        globalDecls   = Nil,
        assertions    = Nil)

    VCCRepair.run(benchmark, synthesisConfig)

  }

}
