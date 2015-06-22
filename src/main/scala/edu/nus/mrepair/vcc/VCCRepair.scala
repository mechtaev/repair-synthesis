package edu.nus.mrepair.vcc

import edu.nus.mrepair._
import edu.nus.mrepair.Utils._
import java.io.File
import edu.nus.vctrans.subsmode
import edu.nus.mrepair.vcc.translation.{AnnotationFilter, RepairableSubstitutor, TFGenerator}
import edu.nus.mrepair.Utils.SimpleLogger._
import org.smtlib.IExpr

/**
  * VCC benchmark runner
  */
object VCCRepair {

  def run(benchmark: VCCBenchmark, synthesisConfig: SynthesisConfig): BenchmarkExecutionResult = {

    Utils.logBenchmarkName = benchmark.name
    Utils.logBenchmarkVersion = benchmark.version
    Utils.resetTimeLog // processing vc
        
    val file = new File(benchmark.smtFile)
    val script = SMTParser.parseFile(file)

    if (Utils.verbose) println("[info] transforming formula...")
    val rawTF = TFGenerator.translate(script, false, benchmark.globalDecls)
    if (Utils.enableLogging) rawTF.toString.log("tf-raw.log")

    val tfWithFixedAnnotations = AnnotationFilter.filter(rawTF)
    if (Utils.enableLogging) tfWithFixedAnnotations.toString.log("tf-fixed-annotations.log")

    val declsIndex = VCCUtils.buildDeclsIndex(tfWithFixedAnnotations.declarations)
    val (tf, repairable) = RepairableSubstitutor.substitute(tfWithFixedAnnotations, declsIndex, benchmark.suspicious, benchmark.entryFunction, synthesisConfig)
    if (Utils.enableLogging) tf.toString.log("tf-substituted.log")
    if (Utils.enableLogging) (prettyList(repairable.inGlobalDecls.map(_.toString())) + "\n" + prettyList(repairable.byFunction.toList.map(_.toString()))).log("tf-repairable.log")
    val numLocations = repairable.inGlobalDecls.size + repairable.byFunction.toList.map(_._2.size).foldLeft(0)(_ + _)

    Utils.logTime // generating rc

    //should we pass TF assertions?
    val (rc, components) = 
      RCGenerator.generate(tf, repairable, benchmark.entryFunction, benchmark.tests, benchmark.suspicious, benchmark.assertions, synthesisConfig)

    Utils.logTime // loading rc

    val (patch, stat) = RCGenerator.solve(rc, repairable, components, synthesisConfig)
    
    Utils.logTime //done

    Utils.getTimeLog match {
      case vcTime :: tfTime :: rcTime :: simplificationTime :: solvingTime :: Nil =>

        patch match {
          case Right(isSolvingTimeout) =>
            BenchmarkExecutionResult(benchmark = benchmark.name,
                                     version = benchmark.version,
                                     config = synthesisConfig,
                                     vcProcessingTime = vcTime,
                                     tfProcessingTime = tfTime,
                                     rcProcessingTime = rcTime,
                                     rcSimplificationTime = simplificationTime,
                                     rcSolvingTime = solvingTime,
                                     fixed = false,
                                     isTimeout = isSolvingTimeout,
                                     verified = false,
                                     patch = Nil,
                                     date = (new java.util.Date).toString,
                                     localization = benchmark.localization,
                                     locations = numLocations,
                                     solverStat = stat)
          case Left(diff) =>
            val p = diff.map({ case (buggy, fixed) => buggy.toString + " --> " + fixed.toString })
            val stopSolvingTime = System.currentTimeMillis
            BenchmarkExecutionResult(benchmark = benchmark.name,
                                     version = benchmark.version,
                                     config = synthesisConfig,
                                     vcProcessingTime = vcTime,
                                     tfProcessingTime = tfTime,
                                     rcProcessingTime = rcTime,
                                     rcSimplificationTime = simplificationTime,
                                     rcSolvingTime = solvingTime,
                                     fixed = true,
                                     isTimeout = false,
                                     verified = true,
                                     patch = p,
                                     date = (new java.util.Date).toString,
                                     localization = benchmark.localization,
                                     locations = numLocations,
                                     solverStat = stat)
        }
    }
    
  }

}
