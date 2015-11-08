package edu.nus.mrepair

import edu.nus.mrepair.decision._
import edu.nus.mrepair.synthesis.{ProgramFormula, Formula}
import edu.nus.mrepair._
import ProgramFormula._
import ProgramFormula.ConstantConversion._
import edu.nus.mrepair.vcc._
import edu.nus.mrepair.vcc.VCCUtils._
import java.io.File
import edu.nus.mrepair.synthesis.Formula
import Formula.{Variable, IntegerType, IntegerValue}
import edu.nus.mrepair.vcc.drivers._
import edu.nus.mrepair.klee.AFParser
import edu.nus.mrepair.klee.AFRepair

object Driver {

  case class ProgramOptions(benchmark: String = "",
                            version: String = "",
                            logging: Boolean = false,
                            localization: Boolean = false,
                            quiet: Boolean = false,
                            config: File = new File("default.json"))

  def isWindows = System.getProperty("os.name").startsWith("Windows")

  def main(args: Array[String]) {

    if (isWindows) {
      System.loadLibrary("libz3")
      System.loadLibrary("libz3java")
    }

    // for DirectFix:

    // val parser = new scopt.OptionParser[ProgramOptions]("repair-maxsat") {
    //   head("repair-maxsat", "0.0")
    //   opt[String]('b', "benchmark") required() action { (x, c) =>
    //     c.copy(benchmark = x) } text "benchmark name"
    //   opt[String]('v', "version") required() action { (x, c) =>
    //     c.copy(version = x) } text "benchmark version"
    //   opt[File]('c', "config") valueName "<file>" action { (x, c) =>
    //     c.copy(config = x) } text "configuration file"
    //   opt[Unit]("log") action { (_, c) =>
    //     c.copy(logging = true) } text "enable logging"
    //   opt[Unit]("localization") action { (_, c) =>
    //     c.copy(localization = true) } text "enable localization"
    //   opt[Unit]('q', "quiet") action { (_, c) =>
    //     c.copy(quiet = true) } text "disable stdout messages"
    //   help("help") text "prints this usage text"
    // }

    // val Some(options) = parser.parse(args, ProgramOptions())

    // Utils.verbose = !options.quiet

    // if (options.logging) Utils.enableLogging = true

    // val configuration = Report.parseConfig(options.config)

    // val result = options.benchmark match {
    //   case "callee"    => CalleeDriver.run(options.version, configuration, options.localization)
    //   case "tcas"      => TcasDriver.run(options.version, configuration, options.localization)
    //   case "schedule"  => ScheduleDriver.run(options.version, configuration, options.localization)
    //   case "schedule2" => Schedule2Driver.run(options.version, configuration, options.localization)
    //   case "coreutils" => CoreutilsDriver.run(options.version, configuration, options.localization)
    //   case "replace"   => ReplaceDriver.run(options.version, configuration, options.localization)
    // }

    // println(Report.toString(result))


    // for Angelix:


    //TODO it is a hack. actually it should be fixed in maxsmt-playgroud
    // val dir = new File("log/")
    // if(!dir.exists()) dir.mkdirs()

    Utils.enableLogging = false

    // Usage:
    // synthesis <angelic-forest-file> <extacted-dir> <output-patch-file> <config-file> <level>+
    //   exitcode: 0 unless error
    //   stdout: SUCCESS, FAIL, TIMEOUT
    //   stderr: logging

    val angelicForestFile :: extractedDir :: outputFile :: configFile :: Nil = args.toList

    val config = Report.parseConfig(new File(configFile))
    //val angelicForest = AFRepair.generateAngelicForest(smtFiles, testUniverseDir, testSuiteIds)
    val angelicForest = AFParser.parse(new File(angelicForestFile))
    val (patch, ids) = AFRepair.generatePatch(config, extractedDir, angelicForest)
    var patchString = ""
    patch match {
      case Right(isSolvingTimeout) =>
        if (isSolvingTimeout) {
          println("TIMEOUT")
        } else {
          println("FAIL")
        }
      case Left(diff) =>
        println("SUCCESS")
        diff.groupBy(_._1).foreach({
          case (_, (id, inst, oldE, newE) :: _) =>
            //TODO either sort of drop instances
            patchString = patchString + ids(id) + "\n" + oldE + "\n" + newE + "\n"
        })
        Utils.writeToFile(outputFile, patchString)
        return
    }
  }

}
