package plainer.utils

import java.io.PrintWriter
import java.io.File
import scala.io.Source

  /**
   * If this is Java, the code will be something like this:
   
  String filePath = "Path/To/Your/Text/File.txt";
  try {
      BufferedReader lineReader = new BufferedReader(new FileReader(filePath));
      String lineText = null;
      while ((lineText = lineReader.readLine()) != null) {
          System.out.println(lineText);
      }
    lineReader.close();
  } catch (IOException ex) {
    System.err.println(ex);
  }
  */

object FileReader extends App {
  def readWrite = {
    val path = List("home", "eddie", "Downloads").mkString("/", "/", "/")
    val destination = new File(path + "output.csv")
    destination.createNewFile()
    val writer = new PrintWriter(destination)
    for(line <- Source.fromFile(path + "file.csv").getLines()) {
      val splitted = line.split(",")
      println(splitted.mkString("[", "|", "]"))
      splitted.tail foreach { piece => 
        writer.println(List(splitted.head,piece).mkString("",";",";"))
      }
    }
    writer.close()
  }
  
  def writerZ(destination: File, content: String) = {
    withPrintWriter(destination) {
      println("writing: %s".format(content))
      writer => writer.write(content)
    }
  }
  
  def withPrintWriter(file: File)(op: PrintWriter => Unit) {
    val writer = new PrintWriter(file)
    try {
      op(writer)
    } finally {
      writer.close()
    }
  }
  
  def loanPattern = {
    val file = new File("/home/eddie/Downloads/date.txt")
    withPrintWriter(file) {
      writer => writer.println(new java.util.Date)
    }
  }
  
  def runIterator = {
    val it: Iterator[Int] = Iterator.range(1, 100)
    while (it.hasNext) {
      val x = it.next
      println(x * x)
    }
  }
  
  readWrite
  loanPattern
}