package edu.nus.mrepair

import java.io.File
import java.util.Calendar
import java.text.SimpleDateFormat
import argonaut._
import Argonaut._
import argonaut.EncodeJson
import edu.nus.mrepair.synthesis._
import edu.nus.mrepair.decision.SolverStat
import edu.nus.mrepair.SynthesisConfig
import edu.nus.mrepair.BenchmarkExecutionResult
import edu.nus.mrepair.SuspiciousLocations
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
        case "alternatives"           => Alternatives()
        case "integer-constants"      => IntegerConstants()
        case "boolean-constants"      => BooleanConstants()
        case "variables"              => Variables()
        case "basic-arithmetic"       => BasicArithmetic()
        case "basic-logic"            => BasicLogic()
        case "basic-inequalities"     => BasicInequalities()
        case "extended-arithmetic"    => ExtendedArithmetic()
        case "extended-logic"         => ExtendedLogic()
        case "extended-inequalities"  => ExtendedInequalities()
        case "mixed-conditional"      => MixedConditional()
        case "conditional-arithmetic" => ConditionalArithmetic()
      }
    })

  implicit def ComponentLevelEncodeJson: EncodeJson[ComponentLevel] =
    EncodeJson({
      case Alternatives()          => jString("alternatives")
      case IntegerConstants()      => jString("integer-constants")
      case BooleanConstants()      => jString("boolean-constants")
      case Variables()             => jString("variables")
      case BasicArithmetic()       => jString("basic-arithmetic")
      case BasicLogic()            => jString("basic-logic")
      case BasicInequalities()     => jString("basic-inequalities")
      case ExtendedArithmetic()    => jString("extended-arithmetic")
      case ExtendedLogic()         => jString("extended-logic")
      case ExtendedInequalities()  => jString("extended-inequalities")
      case MixedConditional()      => jString("mixed-conditional")
      case ConditionalArithmetic() => jString("conditional-arithmetic")
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
    casecodec7(SynthesisConfig.apply,
               SynthesisConfig.unapply)("encodingConfig",
                                        "simplification",
                                        "reuseStructure",
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
