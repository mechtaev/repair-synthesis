package edu.nus.mrepair.vcc.drivers

import scala.util.parsing.combinator._
import edu.nus.mrepair.synthesis.ProgramFormula
import ProgramFormula._
import ProgramFormula.ConstantConversion._
import java.io.File
import edu.nus.mrepair._
import edu.nus.mrepair.vcc._
import scala.collection.JavaConverters._
import edu.nus.mrepair.synthesis.{EncodingConfig, Linear}
import edu.nus.mrepair.SuspiciousLocations
import edu.nus.mrepair.synthesis.EncodingConfig
import edu.nus.mrepair.VCCBenchmark
import edu.nus.mrepair.SynthesisAST
import edu.nus.vctrans.ast.DefineFunAst
import edu.nus.mrepair.SMTParser
import org.smtlib.command.{C_assert, C_define_fun}
import org.smtlib.IExpr

object TcasDriver extends AbstractDriver {

  def benchmarkName = "tcas"

  def run(benchmarkVersion: String, synthesisConfig: SynthesisConfig, localization: Boolean): BenchmarkExecutionResult = {

    val benchmark = VCCBenchmark(name          = benchmarkName,
                                 version       = benchmarkVersion,
                                 entryFunction = "alt_sep_test",
                                 smtFile       = vcFile(benchmarkVersion),
                                 tests         = selectTestsByVersion(benchmarkVersion),
                                 localization  = localization,
                                 suspicious    = suspiciousByVersion(benchmarkVersion, localization),
                                 globalDecls   = loadGlobalDecls(benchmarkVersion),
                                 assertions    = Nil)

    VCCRepair.run(benchmark, synthesisConfig)
  }

  def selectTestsByVersion(version: String): List[List[TestData]] = {
    selectTestsIndexesByVersion(version).map((id: String) => inputOutputRelationship(getTestData(id)) :: Nil)
  }

  def selectTestsIndexesByVersion(version: String): List[String] = {
    val firstN = (n: Int) => (1 to n).toList.map(_.toString)
    val interval = (s: Int, e: Int) => (s to e).toList.map(_.toString)
    val all = firstN(1578) //total 1608

    object V1 {
      //it is minimal. Why?
      val ts1 = List(1, 2, 3, 4, 9, 10, 12, 104).map(_.toString)
      //here we replaced 9 by 5-8, it does not work. Why?
      val ts2 = List(1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 104).map(_.toString)

      val ts3 = firstN(50)
    }

    object V10 {
      val v10_int = interval(1300, 1400)
      val allFailing =
        List(1323, 1324, 1325, 1326, 1327, 1328, 1329, 1330, 1400, 557, 870, 878, 906).map(_.toString)
    }

   // version match {
   //   case "1" => "1" :: Nil
   //   case "10" => V10.allFailing
   // }

    loadSemfixTests(version).map(_.toString)
  }

  def suspiciousByVersion(version: String, localize: Boolean): SuspiciousLocations = {
    //TODO consider global variable declarations and ALIM function here;
    //for ALIM we need to make sure that renaming is done correctly for several instances. 
    val allFunctions = SuspiciousLocations(List("alt_sep_test",
                                                "Non_Crossing_Biased_Climb",
                                                "Non_Crossing_Biased_Descend",
                                                "Inhibit_Biased_Climb",
                                                "Own_Below_Threat",
                                                "Own_Above_Threat",
                                                "ALIM"),
                         globalVariables = true)
    if (! localize) return allFunctions
    version match {
      case "1" => SuspiciousLocations(List("Non_Crossing_Biased_Climb"), globalVariables = false)
      case "2" => SuspiciousLocations(List("Inhibit_Biased_Climb"), globalVariables = false)
      case "3" => SuspiciousLocations(List("alt_sep_test"), globalVariables = false)
      case "4" => SuspiciousLocations(List("Non_Crossing_Biased_Climb"), globalVariables = false)
      case "5" => SuspiciousLocations(List("alt_sep_test"), globalVariables = false)
      case "6" => SuspiciousLocations(List("Own_Below_Threat"), globalVariables = false)
      case "7" => SuspiciousLocations(Nil, globalVariables = true) // array init
      case "8" => SuspiciousLocations(Nil, globalVariables = true) // array init 
      case "9" => SuspiciousLocations(List("Non_Crossing_Biased_Descend"), globalVariables = false)
      case "10" => SuspiciousLocations(List("Own_Below_Threat", "Own_Above_Threat"), globalVariables = false)
      case "11" => SuspiciousLocations(List("Own_Below_Threat", "Own_Above_Threat"/*, "alt_sep_test"*/), globalVariables = false)
      case "12" => SuspiciousLocations(List("alt_sep_test"), globalVariables = false)
      case "13" => SuspiciousLocations(Nil, globalVariables = true) // array init
      case "14" => SuspiciousLocations(Nil, globalVariables = true) // array init
      case "15" => SuspiciousLocations(List("alt_sep_test"), globalVariables = true)
      case "16" => SuspiciousLocations(Nil, globalVariables = true) // array init
      case "17" => SuspiciousLocations(Nil, globalVariables = true) // array init
      case "18" => SuspiciousLocations(Nil, globalVariables = true) // array init
      case "19" => SuspiciousLocations(Nil, globalVariables = true) // array init
      case "20" => SuspiciousLocations(List("Non_Crossing_Biased_Climb"), globalVariables = false)
      case "21" => SuspiciousLocations(List("Non_Crossing_Biased_Climb"), globalVariables = false)
      case "22" => SuspiciousLocations(List("Non_Crossing_Biased_Climb"), globalVariables = false)
      case "23" => SuspiciousLocations(List("Non_Crossing_Biased_Descend"), globalVariables = false)
      case "24" => SuspiciousLocations(List("Non_Crossing_Biased_Descend"), globalVariables = false)
      case "25" => SuspiciousLocations(List("Non_Crossing_Biased_Descend"), globalVariables = false)
      case "26" => SuspiciousLocations(List("alt_sep_test"), globalVariables = false)
      case "27" => SuspiciousLocations(List("alt_sep_test"), globalVariables = false)
      case "28" => SuspiciousLocations(List("Inhibit_Biased_Climb"), globalVariables = false)
      case "29" => SuspiciousLocations(List("Inhibit_Biased_Climb"), globalVariables = false)
      case "30" => SuspiciousLocations(List("Inhibit_Biased_Climb"), globalVariables = false)
      case "31" => SuspiciousLocations(List("Non_Crossing_Biased_Climb", "alt_sep_test"), globalVariables = false)
      case "32" => SuspiciousLocations(List("Non_Crossing_Biased_Descend", "alt_sep_test"), globalVariables = false)
      case "33" => SuspiciousLocations(Nil, globalVariables = true) // array init
      case "34" => SuspiciousLocations(List("alt_sep_test"), globalVariables = false)
      case "35" => SuspiciousLocations(List("Inhibit_Biased_Climb"), globalVariables = false)
      case "36" => SuspiciousLocations(List("alt_sep_test"), globalVariables = false)
      case "37" => SuspiciousLocations(List("ALIM"), globalVariables = false)
      case "38" => SuspiciousLocations(Nil, globalVariables = true) // array declaration
      case "39" => SuspiciousLocations(List("Non_Crossing_Biased_Descend"), globalVariables = false)
      case "40" => SuspiciousLocations(List("Non_Crossing_Biased_Climb", "alt_sep_test"), globalVariables = false)
      case "41" => SuspiciousLocations(List("Non_Crossing_Biased_Climb"), globalVariables = false)
    }
  }

  def inputOutputRelationship(input: TcasInput): TestData = {
    SynthesisAST(
      (ivar("Global#Cur_Vertical_Sep@$s")           === input("Cur_Vertical_Sep"))
    & (ivar("Global#High_Confidence@$s")            === input("High_Confidence"))
    & (ivar("Global#Two_of_Three_Reports_Valid@$s") === input("Two_of_Three_Reports_Valid"))
    & (ivar("Global#Own_Tracked_Alt@$s")            === input("Own_Tracked_Alt"))
    & (ivar("Global#Own_Tracked_Alt_Rate@$s")       === input("Own_Tracked_Alt_Rate"))
    & (ivar("Global#Other_Tracked_Alt@$s")          === input("Other_Tracked_Alt"))
    & (ivar("Global#Alt_Layer_Value@$s")            === input("Alt_Layer_Value"))
    & (ivar("Global#Up_Separation@$s")              === input("Up_Separation"))
    & (ivar("Global#Down_Separation@$s")            === input("Down_Separation"))
    & (ivar("Global#Other_RAC@$s")                  === input("Other_RAC"))
    & (ivar("Global#Other_Capability@$s")           === input("Other_Capability"))
    & (ivar("Global#Climb_Inhibit@$s")              === input("Climb_Inhibit"))
    & (ivar("L#alt_sep@1")                          === input("output")))
  }

  def getTestData(id: String): TcasInput = {
    val dir = "Subjects" + File.separator + "Tcas" + File.separator + "outputs" + File.separator + "correct"
    val testFile = dir + File.separator + "t" + id
    val testSource = scala.io.Source.fromFile(testFile)
    val content = testSource.mkString
    val input = TcasInputParser(content)
    testSource.close()
    input
  }

  type TcasInput = Map[String, Int]

  object TcasInputParser extends JavaTokenParsers {
    def apply(input: String): TcasInput = parseAll(inputs, input).get.toMap
    def inputs = rep(input)
    def input = inputName ~ "=" ~ inputValue ^^ { case n ~ "=" ~ v => (n, v) }
    def inputName: Parser[String] = ident
    def inputValue: Parser[Int] = wholeNumber ^^ { case si => Integer.parseInt(si) }
  }

}
