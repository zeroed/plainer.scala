package plainer.external

import org.scalatest.FunSuite
import com.sun.corba.se.pept.transport.OutboundConnectionCache

/**
 * Write a function that takes two parameters, a string and an integer. 
 * The function will return another string that is similar to the input 
 * string, but with certain characters removed. 
 * It's going to remove characters from consecutive runs of the same 
 * character, where the length of the run is greater than the input parameter.
 * 
 * 1.Ex: "aaab", 2 => "aab"
 * 2.Ex: "aabb", 1 => "ab"
 * 3.Ex: "aabbaa", 1 => "aba"
 * 
 */

object Question {

  object Util {
    def countConsecutives(input: String, sum: Int = 0): Int = {
      input.toList match {
        case (x :: y :: rest) => { if (x == y) {
            println("%s and %s are doubles!".format(x, y))
            countConsecutives((y :: rest).mkString, sum + 1)
          }
          else { 
            println("Stopping at %s".format(sum))
            sum }
        }
        case _ => sum
      }
    }
  }
  
  trait Solver {
    def removeExtraConsecutives(input: String, maxConsecutive: Int): String
  }
  
  object AlaScala extends Solver {
    /**
     * Recursive, no matter what!
     */
    private def pairer(input: String, limit: Int, result: String = "", sum: Int = 0): String = {
      assert(limit > 0)
      println("input: %s with limit %s result: '%s' and sum count: %s".format(input, limit, result, sum))
      input.toList match {
        case (List(r)) => {
          println("  A monolist: [%s] with result: %s.".format(List(r).mkString, result))
          if (sum < limit) (result :+ r).mkString else result
        }
        case (List()) => {
          println("  End, so the result is: %s".format(result))
          result
        }
        case (x :: rest) => {
          println("  %s :: %s and result: '%s'".format(x, rest.mkString(","), result))
          if(x == rest.head && sum < limit) {
            //attach and sum
            println("  %s and %s are equals, we have still space, so %s goes to the result".format(x, rest.head, x))
            pairer(rest.mkString, limit, result + x, sum+1)
          } else if(x == rest.head && sum >= limit) {
            //skip and sum
            println("  %s and %s are equals, we doesn't have space, so %s skip the result".format(x, rest.head, x))
            pairer(rest.mkString, limit, result, sum+1)
          } else if(sum < limit) {
            //attach and reset
            println("  %s and %s are not equals, end of strike, there is space, so %s goes to the result".format(x, rest.head, x))
            pairer(rest.mkString, limit, result + x, 0)
          } else {
            //skip and reset
            println("  %s and %s are not equals, it is not a strike, so %s skip the result".format(x, rest.head, x))
            pairer(rest.mkString, limit, result, 0)
          }
        }
      }
    }
    
    override def removeExtraConsecutives(input: String, maxConsecutive: Int): String = {
      pairer(input, maxConsecutive)
    }
  }
  
  object AlaPython extends Solver {
    /**
     * Python implementation 
     * 
        01.def remove_extra_consecutive(input_str, max_consecutive_chars):
        02.output, prev_char, current_char_seen = '', None, 0
        03.for current_char in input_str:
        04.  if current_char == prev_char:
        05.    current_char_seen += 1
        06.  else:
        07.    current_char_seen = 0
        08.    prev_char = current_char
        09.if current_char_seen < max_consecutive_chars:
        10.  output += current_char
        11.return output
    * 
    */
    private def pairer(input: String, maxConsecutiveChars: Int): String = {
      var output = ""
      var previousChar: Option[Char] = None
      var currentCharSeen = 0
      for(currentChar <- input.toList) {
        if (currentChar == (previousChar.getOrElse(None))) currentCharSeen += 1
        else {
          currentCharSeen = 0
          previousChar = Option[Char](currentChar)
        }
        if (currentCharSeen < maxConsecutiveChars) output += currentChar
      }
      output
    }
   override def removeExtraConsecutives(input: String, maxConsecutive: Int): String = {
      pairer(input, maxConsecutive)
    }
  }
}

object TryQuestion extends App {
  override def main(args: Array[String]) {
    println(Question.AlaScala.removeExtraConsecutives("aaab", 2))
    println("-" * 40)
    println(Question.AlaScala.removeExtraConsecutives("aabb", 1))
    println("-" * 40)
    println(Question.AlaScala.removeExtraConsecutives("aabbaa", 2))
    println("-" * 40)
  }
}

class QuestionTest extends FunSuite {
 
  test("Question works A la Scala") {
    assert(Question.AlaScala.removeExtraConsecutives("", 2) == "")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("ab", 1) == "ab")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("aba", 1) == "aba")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("abbba", 2) == "abba")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("aabbaa", 1) == "aba")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("aaab", 2) == "aab")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("aaab", 1) == "ab")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("aabbaa", 1) == "aba")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("aabbaa", 2) == "aabbaa")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("aaab", 3) == "aaab")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("aaabbbb", 1) == "ab")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("aabbaaa", 2) == "aabbaa")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("ababababa", 2) == "ababababa")
    println("-" * 40)
    assert(Question.AlaScala.removeExtraConsecutives("aaaaaaaaa", 1) == "a")
    println("-" * 40)
  }
  
  test("Question works A la Python") {
    assert(Question.AlaPython.removeExtraConsecutives("", 2) == "")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("ab", 1) == "ab")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("aba", 1) == "aba")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("abbba", 2) == "abba")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("aabbaa", 1) == "aba")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("aaab", 2) == "aab")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("aaab", 1) == "ab")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("aabbaa", 1) == "aba")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("aabbaa", 2) == "aabbaa")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("aaab", 3) == "aaab")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("aaabbbb", 1) == "ab")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("aabbaaa", 2) == "aabbaa")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("ababababa", 2) == "ababababa")
    println("-" * 40)
    assert(Question.AlaPython.removeExtraConsecutives("aaaaaaaaa", 1) == "a")
    println("-" * 40)
  }
}
