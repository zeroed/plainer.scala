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

/**
 * @author eddie
 *
 */

object Main extends App {

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
        val html = Source.fromURL(url)
            Some(html.mkString)
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
  
  def printLinesFromFile(filename: String): Unit = {
    for(line <- Source.fromFile(filename).getLines()) println(line)
  }
  
  def mapLineForLength(filename: String): Map[Int, Int] = {
    //for(line <- Source.fromFile(filename).getLines)
    //list.view.zipWithIndex foreach {case (value,index) => println(value,index)}
    //for (i <- a.indices) yield myFn(a(i),i)
    Source.fromFile(filename).getLines.zipWithIndex foreach {
      case (value,index) => println(value,index)
    }
    Map()
  }

  override def main(args: Array[String]) {
    val now = new Date
    val dateInstance = getDateInstance(LONG, Locale.US)
    println(dateInstance format now)  

    println(formatArgs(args))

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

    toInt("42") match {
    case Some(i) => println(i)
    case None => println("That didn't work.")
    }

    val b = new Box("a", 1)
    println(b)

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

    oncePerSecond(() => println("time flies like an arrow..."))

    val stamp = new Stamp("stampMe")
    println(stamp(4))

    printUrlContent("foo")

    println(getTitleFromUrl("http://scala-lang.org"))

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
    
    println("Yup... it's THAT Ulysses...")
    mapLineForLength("/home/eddie/Documents/Ulysses.txt")
  }
}

