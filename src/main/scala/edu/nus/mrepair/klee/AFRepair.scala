package edu.nus.mrepair.klee

import edu.nus.mrepair._
import edu.nus.mrepair.AngelicFix._
import edu.nus.mrepair.Utils._
import java.io.File
import edu.nus.mrepair.Utils.SimpleLogger._
import org.smtlib.IExpr

/**
  * AngelicFix implementation top class
  */

object AFRepair {


  //TODO: what do we need to pass to this function to get complete information about the subject program?

  def run(synthesisConfig: SynthesisConfig): Int = {
    ???
  }

  def generatePatch(f: File) {
    val af = readAngelicValues(f)
    ???
  }

  /**
    * Reading angelic values from file
    */
  def readAngelicValues(f: File): AngelicForest = {
    ???
  }


}
