package edu.nus.mrepair

import java.io.{IOException, FileReader, FileWriter, File}
import java.util.UUID
import org.smtlib.ICommand.IScript
import org.smtlib.{SMT, ISource, ICommand, IParser}
import org.smtlib.impl.Pos
import edu.nus.mrepair.Utils

/**
 * Created by seryozha on 4/10/14.
 */
object SMTParser {

  def parseFile(smtlib2File: File): IScript = {
    val uuid = UUID.randomUUID()
    val tmpFile: File = new File(uuid.toString() + ".smt2")
    makeScript(smtlib2File, tmpFile)

    val smt: SMT = new SMT
    val src: ISource = smt.smtConfig.smtFactory.createSource(smt.smtConfig, tmpFile)
    val parser: IParser = smt.smtConfig.smtFactory.createParser(smt.smtConfig, src)
    var script: ICommand.IScript = null
    try {
      script = parser.parseScript
    }
    catch {
      case e: IParser.ParserException => {
        throw new Error("Failed to parse")
      }
      case e: IOException => {
        throw new Error("Failed to parse")
      }
    }
    assert(script != null)
    tmpFile.delete()

    script
  }
  
  private def makeScript(smt2File: File, scriptFile: File) {
    var input: FileReader = null
    var output: FileWriter = null
    try {
      assert(smt2File.exists, smt2File.getName + " does not exist")
      input = new FileReader(smt2File)
      output = new FileWriter(scriptFile)
      output.write("(\n")
      val buf: Array[Char] = new Array[Char](1024)
      var bytesRead: Int = 0
      while ( {
        bytesRead = input.read(buf);
        bytesRead
      } > 0) {
        output.write(buf, 0, bytesRead)
      }
      output.write("\n)")
    }
    finally {
      input.close()
      output.close()
    }
  }

}
