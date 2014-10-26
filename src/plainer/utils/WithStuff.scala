/**
 *
 */
package plainer.utils

import java.io.PrintWriter
import java.io.File

/**
 * @author eddie
 *
 */
object WithStuff {
  
  def withPrintWriter(file: File, op: PrintWriter => Unit) {
    val writer = new PrintWriter(file) 
    try { 
      op(writer) 
    } finally { 
      writer.close() 
    } 
  }
  
  def withLogs(op: () => Any) {
    println("Start")
    op()
    println("End")
  }
}