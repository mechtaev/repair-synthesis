package edu.nus.mrepair.klee

import edu.nus.mrepair.AngelicFix._
import java.io.File
import argonaut._
import Argonaut._
import argonaut.EncodeJson
import argonaut.JString
import argonaut.JNumber
import argonaut.JBool

object AFParser {


  implicit def VariableValueDecodeJson: DecodeJson[VariableValue] =
    DecodeJson(c => for {
      name <- (c --\ "name").as[String]
      value <- (c --\ "value").as[Json]
    } yield {
      if (value.isBool) {
        BoolVal(name, value.bool.get)
      } else {
        IntVal(name, value.number.get.toInt)
      }
    })

  implicit def VariableValueEncodeJson: EncodeJson[VariableValue] =
    EncodeJson({
      case IntVal(name, i) =>
        ("name" := name) ->:
        ("value" := i) ->: jEmptyObject
      case BoolVal(name, b) =>
        ("name" := name) ->:
        ("value" := b) ->: jEmptyObject
    })

  implicit def AngelicValueCodecJson : CodecJson[AngelicValue] =
    casecodec4(AngelicValue.apply,
               AngelicValue.unapply)("context", "value", "expression", "instId")


  def parse(file: File): AngelicForest = {
    val afSource = scala.io.Source.fromFile(file)
    val str = afSource.mkString
    val Some(af) = str.decodeOption[AngelicForest]
    af
  }

}
