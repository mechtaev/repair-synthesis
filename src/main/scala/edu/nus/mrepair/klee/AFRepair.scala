package edu.nus.mrepair.klee

import edu.nus.mrepair._
import edu.nus.mrepair.AngelicFix._
import edu.nus.mrepair.Utils._
import java.io.File
import edu.nus.mrepair.Utils.SimpleLogger._
import org.smtlib.IExpr
import scala.util.parsing.combinator._
import com.microsoft.z3._
import edu.nus.maxsmtplay._
import scala.collection.JavaConverters._
import edu.nus.mrepair.synthesis.{ProgramFormula, Formula, ComponentFormula}
import Formula._
import ProgramFormula._
import org.smtlib.command.C_assert
import edu.nus.mrepair.vcc.VCCUtils


/**
  * AngelicFix implementation top class
  */
object AFRepair {

  def generatePatch(synthesisConfig: SynthesisConfig,
                    extractedDir: String,
                    angelicForest: AngelicForest): (Either[List[(Int, Int, ProgramFormulaExpression, ProgramFormulaExpression)], Boolean], List[String]) = {
    //TODO I need to be more clear from where I take ids: files or environment variable
    val suspiciousIds = (new File(extractedDir)).listFiles.filter(f => """.*\.smt2$""".r.findFirstIn(f.getName).isDefined).map(_.getName.dropRight(".smt2".length)).toList
    
    val suspicious = suspiciousIds.zipWithIndex.map({
      case (id, index) =>
        val script = SMTParser.parseFile(new File(extractedDir + "/" + id + ".smt2"))
        val expr = script.commands().asScala.toList.map({
          case asrt: C_assert => asrt.expr()
        }).apply(0)
        (index + 1, expr)
    })

    //FIXME: (index + 1) is magic, without which it does not work (probably, I used id=0 for something else)

    val (rc, oldExpr, components) = edu.nus.mrepair.klee.RCGenerator.generate(angelicForest, suspicious, "" :: suspiciousIds, synthesisConfig)

    val (patch, stat) = edu.nus.mrepair.klee.RCGenerator.solve(rc, components, oldExpr, synthesisConfig)

    (patch, "" :: suspiciousIds)
  }

}
