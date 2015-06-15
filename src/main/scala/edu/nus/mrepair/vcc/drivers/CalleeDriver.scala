package edu.nus.mrepair.vcc.drivers

import edu.nus.mrepair.synthesis.ProgramFormula
import ProgramFormula._
import ProgramFormula.ConstantConversion._
import java.io.File
import edu.nus.mrepair._
import edu.nus.mrepair.Utils
import edu.nus.mrepair.{SynthesisAST, SuspiciousLocations, VCCBenchmark}
import edu.nus.mrepair.synthesis.{EncodingConfig, Linear}
import edu.nus.mrepair.SynthesisConfig
import edu.nus.mrepair.BenchmarkExecutionResult
import edu.nus.mrepair.vcc.VCCRepair

object CalleeDriver extends AbstractDriver {

  def benchmarkName = "callee"

  def run(benchmarkVersion: String, synthesisConfig: SynthesisConfig, localization: Boolean): BenchmarkExecutionResult = {

    val calleeSuspicious = 
      if (localization)
        SuspiciousLocations(List("callee_test"), globalVariables = false)
      else
        SuspiciousLocations(List("callee_test", "callee"), globalVariables = false)

    val benchmark =
      VCCBenchmark(
        name          = benchmarkName,
        version       = benchmarkVersion,
        entryFunction = "callee_test",
        smtFile       = vcFile(benchmarkVersion),
        tests =
          ( (SynthesisAST((ivar("P#input_i") === 1) & (ivar("L#output_o@0") === 24)) :: Nil)
         :: (SynthesisAST((ivar("P#input_i") === -1) & (ivar("L#output_o@0") === 0)) :: Nil)
         :: Nil),
        localization  = false,
        suspicious    = calleeSuspicious,
        globalDecls   = Nil,
        assertions    = Nil)

    //TODO the problem here is that callee has two instances of a function
    // with repairable expression so we need to implement some support for instances first


    VCCRepair.run(benchmark, synthesisConfig)
  }

}
