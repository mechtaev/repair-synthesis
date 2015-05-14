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

object AngelicFix {

  case class AngelicValue(context: List[(ProgramVariable, IExpr)], v: IExpr, stmtId: Int)

  case class AngelicPath(test: List[TestData], values: AngelicValues)

  type AngelicForest = List[AngelicPath]

  def generatePatch(f: File) {
    val af = readAngelicValues(f)
    val ...
  }

  /**
    * Reading angelic values from file
    */
  def readAngelicValues(f: File): AngelicForest = {}

}
