/**
 *
 */
package plainer

import util.Random.nextInt
import plainer.external.RandomStringGenerator
import plainer.traits.Cache
import scala.collection.mutable.ListBuffer
import org.htmlcleaner.HtmlCleaner
import org.apache.commons.lang3.StringEscapeUtils
import java.net.URL
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._
import plainer.items.Maybe
import plainer.items.Stamp
import plainer.items.Box
import plainer.items.SetCreator
import scala.io.Source
import plainer.utils.ListExtended
import plainer.utils.IntExtended
import java.io.PrintWriter
import java.io.File
import plainer.utils.Buzzers.{FizzBuzzEasy, FizzBuzzMedium, FizzBuzzHard }
  
/**
 * @author eddie
 *
 */

object Main extends App {

  implicit def intToInt(i: Int) = new IntExtended(i)
  implicit def listToListExtended(list: List[Int]) = { new ListExtended(list) }
  
  def timesTwo(i: Int): Int = {
      println("timesTwo")
      i * 2
  }

  def adder(m: Int, n: Int) =  m + n 
  
  def plusOne = {
      (x: Int) => x + 1
  }

  def randomStream(lenght: Int): Stream[Int] = {
    Stream.continually(nextInt(20)).take(lenght)
  }

  def randomList(lenght: Int): List[Int] = {
    randomStream(lenght).toList
  }

  def randomInt = {
    nextInt(20)
  }

  def randomInt(diceSize: Int): Int = {
    nextInt(diceSize)
  }

  def launch(diceSize: Int)(modifier: Int): Int = {
    adder(randomInt(diceSize), modifier)
  }

  def capitalizeAll(args: String*) = {
    args.map { arg =>
    arg.capitalize
    }
  }

  def getRandomString : String = {
      RandomStringGenerator
      .randomStringTailRecursive(nextInt(5), Nil)
      .mkString(", ")
  }

  def getSourceFromURL(url: String): Option[String] = {
      import scala.io.Source
      try {
        Some(Source.fromURL(url).mkString)
      } catch {
        case e: java.net.MalformedURLException => None
      }
  }

  def printUrlContent(url: String) = {
    println(getSourceFromURL(url))
  }

  def toInt(in: String): Option[Int] = {
    try {
      Some(Integer.parseInt(in.trim))
    } catch {
      case e: NumberFormatException => None
    }
  }

  def getTitleFromUrl(url: String): List[String] = {
    var stories = new ListBuffer[String]
      val cleaner = new HtmlCleaner
      val props = cleaner.getProperties
      val elements = try {
        val rootNode = cleaner.clean(new URL(url))
          rootNode.getElementsByName("title", true)
      } catch {
        case e: java.net.UnknownHostException => Array[org.htmlcleaner.TagNode]()
        case e: java.net.SocketException => Array[org.htmlcleaner.TagNode]()
      }
    return elements.map {
      el => el.getText.toString
    }.toList
  }

  def getHeadlinesFromUrl(url: String): List[String] = {
    var stories = new ListBuffer[String]
        val cleaner = new HtmlCleaner
        val props = cleaner.getProperties
        val rootNode = cleaner.clean(new URL(url))
        val elements = rootNode.getElementsByName("a", true)
        for (elem <- elements) {
          val classType = elem.getAttributeByName("class")
              if (classType != null && classType.equalsIgnoreCase("articleTitle")) {
                // stories might be "dirty" with text like "'", clean it up
                val text = StringEscapeUtils.unescapeHtml4(elem.getText.toString)
                    stories += text
              }
        }
    //return stories.filter(storyContainsDesiredPhrase(_)).toList
    return List("this", "is", "just", "a", "sample")
  }

  def oncePerSecond(callback: () => Unit) {
    // while(true) { callback(); Thread sleep 1000 }
    // 1 to 10 foreach { (_) => { callback(); Thread sleep 1000 } }
    for (i <- 1 to 3) { callback(); Thread sleep 500 }
  }

  def timeFlies() {
    println("time flies like an arrow...")
  }

  def printArgs(args: Array[String]): Unit = {
      args.foreach(println)
  }
  
  def formatArgs(args: Array[String]) = args.mkString("\n")
  
  def printSomeLinesFromFile(filename: String, numberOfLines: Int = 10): Unit = {
    assert(numberOfLines > 0)
    for(line <- Source
        .fromFile(filename)
        .getLines
        .take(numberOfLines)) yield { println(line) }
  }
  
  def foldingSum(list: List[Int]): Int = list.foldLeft(0)((r,c) => r + c)
  
  def forPrintLineForLength(filename: String): Unit = {
    //for (i <- a.indices) yield myFn(a(i),i)
    Source.fromFile(filename).getLines.zipWithIndex foreach {
      case (value, index) => println(value, index)
    }
  }
  
  def mapLineForLength(filename: String): scala.collection.mutable.Map[Int, Int] = {
    println("ready to get the line/length map for %s" format filename)
    import scala.collection.mutable.Map

    Source
      .fromFile(filename)
      .getLines
      .zipWithIndex
      .foldLeft(Map[Int, Int]())(
        (r: Map[Int, Int], c: (String, Int)) => {
          if (c._2 isModuleOf 1000) println("Line %d: %s...".format(c._2, c._1.substring(0, c._1.length/4 )))
          r += (c._2 -> c._1.length)
        })
  }
  
  def fileLines(file: java.io.File) = 
    scala.io.Source.fromFile(file).getLines().toList
      
  def filesHere = (new java.io.File(".")).listFiles
  
  def grep(pattern: String) = {
    for {
      file <- filesHere
      if(file != null)
      if file.exists
      if file.isFile
      if file.getName.endsWith(".txt")
      line <- fileLines(file)
      trimmed = line.trim
      if trimmed.matches(pattern)
    } println(file +": "+ trimmed)
  }
  
  def has42(max: Int): List[Int] = {
    if (max < 1) return List[Int]()
    for (
      num <- (1 to max).toList
      if (num isModuleOf(1))
      if (num isModuleOf(2))
      if (num isModuleOf(3))
      if (num isModuleOf(6))
      if (num isModuleOf(7))
      if (num isModuleOf(14))
      if (num isModuleOf(21))
      if (num isModuleOf(42))
    ) yield { println("It's him! %s".format(num)); num }
  }
  
  def intOrString = {
    val in = Console.readLine("Type Either a string or an Int: ")
    val result: Either[String,Int] = try {
        Right(in.toInt)
      } catch {
        case e: Exception =>
          Left(in)
    }
    
    println( result match {
      case Right(x) => "You passed me the Int: " + x + ", which I will increment. " + x + " + 1 = " + (x+1)
      case Left(x) => "You passed me the String: " + x
    })
  }
  
  def cointainsNegativeNumbers(list: List[Int]): Boolean = {
    list.exists(_ > 0)
  }
  
  // Given such a method, you can use it like this: 
  // withPrintWriter( 
  //   newFile("date.txt"), 
  //   writer => writer.println(newjava.util.Date) )

  def filesMatching(query:String, matcher: (String,String) => Boolean) = { 
    for(
        file<-filesHere;
        if matcher(file.getName,query)
    ) yield file 
  }

  def reverse(x: Int, y: Int): (Int, Int) = {
    (-x, -y)
  }
  
  def printSeparator = println("---" * 70)
  
  def getAListWithSomeChanges(limit: Int)(op: (Int, Int) => Int): List[Int] = {
    for {
        i <- (0 until limit).toList
      } yield op(i, limit)
  }
  
  //val intSort = msort((x:Int, y:Int) => x < y)_ 
  //intSort:(List[Int])=>List[Int]=<function1>
  
  override def main(args: Array[String]) {
    
    val Verbose = false
    val now = new Date
    val dateInstance = getDateInstance(LONG, Locale.US)
    
    printSeparator
    
    println(dateInstance format now)  
    println(formatArgs(args))

    if (Verbose) {
      printSeparator
      val lst = List(1, 2, 3, 4, 5)
      val arr = Array(1, 2, 3, 4, 5)
      assert(lst.scanLeft(0)(_ + _) == List(0, 1, 3, 6, 10, 15))
      val res = arr.reduceLeft(
        (a,b) => {
          println("Summing %d + %d".format(a,b))
          a+b
      })
  
      println("ReduceLeft: %d".format(res))
  
      println("RandomInt: %d".format(randomInt))
      println("RandomsList: %s".format(randomList(randomInt)))
      println("RandomInt+1: %d".format(plusOne(randomInt)))
      val curriedLaunch = launch(20)_
      println("Curried launch: %s".format(curriedLaunch))
      println("Full launch: %d".format(curriedLaunch(randomInt)))
  
      val curriedAdd = (adder _).curried
      val addOne = curriedAdd(1)
    }

    if (Verbose) {
      printSeparator
      
      toInt("42") match {
      case Some(i) => println(i)
      case None => println("That didn't work.")
      }
  
      val b = new Box("a", 1)
      println(b)
      println(!b)
      
      val aValue = b.get("a")
      aValue match {
      case Some(aValue) =>
        println("You found %d".format(aValue))
      case None =>
        println("No name value for 'a'")
      }
  
      val bValue = b.get("b")
        bValue match {
        case Some(bValue) =>
          println("You found %d".format(bValue))
        case None =>
          println("No name value 'b'")
      }
  
      b.delete("a")
      println(b)
      b.delete("b")
      println(b)
    }
    
    if (Verbose) {
      printSeparator
      oncePerSecond(() => println("time flies like an arrow..."))
  
      val stamp = new Stamp("stampMe")
      println(stamp(4))
      println(stamp concat "!")
      printUrlContent("foo")
    }

    if (Verbose) {
      printSeparator
      val m = new Maybe("foo", Some(42))
      println("Maybe %s, %s at %s".format(m.name, m.value, m.date))
      m.nullame
      println("Maybe %s, %s at %s".format(m.name, m.value, m.date))
      
      val mutableSet = SetCreator.getMutable("foo", "bar", "lol")
      println("look Ma, i'm a %s".format(mutableSet.getClass()))
      val immutableSet = SetCreator.getImmutable("foo", "bar", "lol") 
      println("look Ma, i'm a %s".format(immutableSet.getClass()))
      
      val treasureMap = scala.collection.mutable.Map[Int, String]()
      treasureMap += (1 -> "Go to island.")
      treasureMap += (2 -> "Find big X on ground.")
      treasureMap += (3 -> "Dig.")
      println(treasureMap(2))
      
      val romanNumeral = scala.collection.immutable.Map(
          1 -> "I", 2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V"
      )
      println(romanNumeral(4))
      
      val x = List.tabulate(5)(n => n * n)
      println(x)

      grep(".*gcd.*")
    }
    
    if (Verbose) {
      printSeparator
      has42(11)
      has42(111)
    }
    
    if (Verbose) {
      printSeparator
      val a = Array(1, 2, 3, 4, 5) // Array(1, 2, 3, 4, 5)
      println((for (e <- a) yield e).mkString("[",",","]")) // Array(1, 2, 3, 4, 5)
      println((for (e <- a) yield { e * 2 }).mkString("[",",","]")) // Array(2, 4, 6, 8, 10)
      println((for (e <- a) yield { e % 2 }).mkString("[",",","]")) // Array(1, 0, 1, 0, 1)
      println((for (e <- a if e > 2) yield { e }).mkString("[",",","]")) // Array(3, 4, 5)
    }
    
    if (true) {
      printSeparator
      def percentageOf(v: Int, over: Int) = { (v * 100)/over }
      val listPartiallyApplied = getAListWithSomeChanges(5) _
      val someChangingFunction: (Int, Int) => Int = (number: Int, upperBound: Int) => { 
        number - scala.util.Random.nextInt(upperBound) }
      val nowAList = listPartiallyApplied(someChangingFunction)
          
      println("Does this random list %s contains negatives? %s"
          .format(
            nowAList.mkString("[",", ","]"), 
            nowAList.hasNegatives))
      
      val bigRun = 1000000
      (0 to 10) foreach { i => 
        print("%02d ".format(i))
        val (dirty, clean) = 
          (0 until bigRun).toList.map {
          _ => listPartiallyApplied(someChangingFunction)
        }.partition(l => l.hasNegatives)
        println("clean: %d (%d)%%, dirty: %d (%d)%%".format(
            clean.length, percentageOf(clean.length, bigRun), 
            dirty.length, percentageOf(dirty.length, bigRun)))
      }
    }
    if (Verbose) {
      plainer.utils.Buzzers.FizzBuzzEasy.run
      plainer.utils.Buzzers.FizzBuzzMedium.run
      plainer.utils.Buzzers.FizzBuzzHard.run
    }
        
    if (Verbose) {
      printSeparator
      println("Yup... it's THAT Ulysses...")
      val ulyssesMap: scala.collection.mutable.Map[Int, Int] = mapLineForLength("/home/eddie/Documents/Ulysses.txt")
      println("The max length is %d".format(ulyssesMap.values.max))
      println("The mean length is %d".format(ulyssesMap.values.sum/ulyssesMap.values.size))
      //print ln(ulyssesMap.values.max)
    }
    printSomeLinesFromFile("/home/eddie/Documents/Ulysses.txt", 42)
    println(getTitleFromUrl("http://scala-lang.org"))
  }
}

