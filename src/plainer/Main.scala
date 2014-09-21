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

/**
 * @author eddie
 *
 */

class Box(paramKey: String, paramValue: Int) extends Cache[String, Int] {
  val key: String = paramKey
  // private var value = None: Option[Int]
//  private var value: Option[Int] = paramValue
  private var value: Option[Int] = Some(paramValue)
  
  def get(paramKey: String): Option[Int] = {
    if (key.equals(paramKey)) value 
    else None
  }
  
  def put(paramKey: String, paramValue: Int) = { 
    if (key.equals(paramKey)) {
      value = Some(paramValue)
      true
    }
    else false
  }
  
  def delete(paramKey: String) = {
      if (key.equals(paramKey)) {
        value = None: Option[Int]
        true
      }
      else false
  }
  
  override def toString: String = {
    "I'm a Box: key %s, value %s".format(key, value)
  }
}

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
	
	def printUrlContent(url: String) = {
	  println("I'm not gonna print %s".format(url))
	  import scala.io.Source
      val html = Source.fromURL("http://scala-lang.org/")
	  val s = html.mkString
	  println(s)
	}
	
	def toInt(in: String): Option[Int] = {
        try {
            Some(Integer.parseInt(in.trim))
        } catch {
            case e: NumberFormatException => None
        }
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

	override def main(args: Array[String]) {
		println("Hello, world!")

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
            println(aValue)
          case None =>
            println("No name value")
        }
		
		val bValue = b.get("b")
		  bValue match {
		  case Some(bValue) =>
	        println(bValue)
		  case None =>
		    println("No name value")
		}

		printUrlContent("foo")
		
	}
}

