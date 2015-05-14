package edu.nus.mrepair

import java.util.concurrent.TimeUnit
import java.io.File

object Utils {

  def writeToFile(file: String, data: String) {
    val pw = new java.io.PrintWriter(new java.io.File(file))
    try {
      pw.write(data)
    } finally {
      pw.close()
    }
  }

  var enableLogging = false
  var verbose = false

  var logBenchmarkName = "undefined"
  var logBenchmarkVersion = "undefined"

  trait LogData {
    def log(file: String): Unit
  }

  object SimpleLogger {
    implicit def stringToLogData(data: String): LogData = {
      new LogData {
        def log(file: String): Unit = {
          val dir = new File("log/" + logBenchmarkName + "/v" + logBenchmarkVersion + "/")
          if(!dir.exists()) dir.mkdirs()
          writeToFile(dir.getAbsolutePath() + "/" + file, data)
        }        
      }
    }
  }

  def prettyList[T](l: List[T]): String = {
    l.map(_.toString).foldLeft("")(_ + "\n" + _)
  }

  def prettyTime(exeTime: Long): String = {
    "%02d:%02d.%03d".format(
      TimeUnit.MILLISECONDS.toMinutes(exeTime),
      TimeUnit.MILLISECONDS.toSeconds(exeTime) -
        TimeUnit.MINUTES.toSeconds(TimeUnit.MILLISECONDS.toMinutes(exeTime)),
      exeTime - TimeUnit.SECONDS.toMillis(TimeUnit.MILLISECONDS.toSeconds(exeTime)))
  }
  
  private var lastTimeStamp = System.currentTimeMillis
  private var savedTimeIntervals = List[Long]()
  
  def logTime: Unit = {
    val curTime = System.currentTimeMillis
    savedTimeIntervals = savedTimeIntervals ++ ((curTime - lastTimeStamp) :: Nil)
    lastTimeStamp = curTime
  }
  
  def getTimeLog = savedTimeIntervals
  
  def resetTimeLog: Unit = {
    savedTimeIntervals = List[Long]()
    lastTimeStamp = System.currentTimeMillis
  } 

}
