package edu.nus.mrepair

import java.io.File
import java.util.Calendar
import java.text.SimpleDateFormat
import argonaut._
import Argonaut._
import argonaut.EncodeJson
import edu.nus.mrepair.synthesis._
import edu.nus.mrepair.decision.SolverStat
import edu.nus.mrepair.vcc.SynthesisConfig
import edu.nus.mrepair.vcc.BenchmarkExecutionResult
import edu.nus.mrepair.vcc.SuspiciousLocations
import argonaut.JString

object Report {

  implicit def EncodingLevelDecodeJson: DecodeJson[EncodingLevel] =
    DecodeJson(c => for {
      str <- c.as[String]
    } yield {
      str match {
        case "linear" => Linear()
        case "logical" => Logical()
        case "nonlinear" => NonLinear()
      }
    })

  implicit def EncodingLevelEncodeJson: EncodeJson[EncodingLevel] =
    EncodeJson({
      case Linear() => jString("linear")
      case Logical() => jString("logical")
      case NonLinear() => jString("nonlinear")
    })

  implicit def ComponentLevelDecodeJson: DecodeJson[ComponentLevel] =
    DecodeJson(c => for {
      str <- c.as[String]
    } yield {
      str match {
        case "constants"    => Constants()
        case "variables"    => Variables()
        case "alternatives" => Alternatives()
        case "booleans"     => Booleans()
        case "comparison"   => Comparison()
        case "arithmetics"  => Arithmetics()
        case "custom"       => Custom()
      }
    })

  implicit def ComponentLevelEncodeJson: EncodeJson[ComponentLevel] =
    EncodeJson({
      case Constants()    => jString("constants")
      case Variables()    => jString("variables")
      case Alternatives() => jString("alternatives")
      case Booleans()     => jString("booleans")
      case Comparison()   => jString("comparison")
      case Arithmetics()  => jString("arithmetics")
      case Custom()       => jString("custom")
    })

  implicit def SuspiciousLocationsCodecJson : CodecJson[SuspiciousLocations] =
    casecodec2(SuspiciousLocations.apply,
               SuspiciousLocations.unapply)("functions", "globalVariables")

  implicit def SolverStatCodecJson : CodecJson[SolverStat] =
    casecodec3(SolverStat.apply,
               SolverStat.unapply)("unsatCores", "soft", "hard")

  implicit def EncodingConfigCodecJson : CodecJson[EncodingConfig] =
    casecodec5(EncodingConfig.apply,
               EncodingConfig.unapply)("level",
                                       "repairIntegerConst",
                                       "repairBooleanConst",
                                       "componentsMultipleOccurrences",
                                       "phantomComponents")

  implicit def SynthesisConfigCodecJson : CodecJson[SynthesisConfig] =
    casecodec6(SynthesisConfig.apply,
               SynthesisConfig.unapply)("encodingConfig",
                                        "simplification",
                                        "spaceReduction",
                                        "solverBound",
                                        "solverTimeout",
                                        "componentLevel")

  implicit def BenchmarkExecutionResultCodecJson:
      CodecJson[BenchmarkExecutionResult] =
    casecodec16(BenchmarkExecutionResult.apply,
                BenchmarkExecutionResult.unapply)("benchmark",
                                                  "version",
                                                  "config",
                                                  "vcProcessingTime",
                                                  "tfProcessingTime",
                                                  "rcProcessingTime",
                                                  "rcSimplificationTime",
                                                  "rcSolvingTime",
                                                  "fixed",
                                                  "isTimeout",
                                                  "verified",
                                                  "patch",
                                                  "date",
                                                  "localization",
                                                  "locations",
                                                  "solverStat")

  def toString(report: BenchmarkExecutionResult): String = {
    report.asJson.spaces2
  }

  def parseConfig(file: File): SynthesisConfig = {
    val configSource = scala.io.Source.fromFile(file)
    val str = configSource.mkString
    val Some(config) = str.decodeOption[SynthesisConfig]
    config
  }

}
