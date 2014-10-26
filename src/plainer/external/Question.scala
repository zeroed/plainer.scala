package plainer.external

import org.scalatest.FunSuite

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
  
  /**
   * Recursive, no matter what!
   */
  def pairer(input: String, limit: Int, result: String = "", sum: Int = 0): String = {
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


  def removeExtraConsecutives(input: String, maxConsecutive: Int): String = {
      pairer(input, maxConsecutive)
  }
}

object TryQuestion extends App {
  override def main(args: Array[String]) {
    println(Question.removeExtraConsecutives("aaab", 2))
    println("-" * 40)
    println(Question.removeExtraConsecutives("aabb", 1))
    println("-" * 40)
    println(Question.removeExtraConsecutives("aabbaa", 2))
    println("-" * 40)
  }
}

class QuestionTest extends FunSuite {
 
  test("Question works") {
    assert(Question.removeExtraConsecutives("", 2) == "")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("ab", 1) == "ab")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("aba", 1) == "aba")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("abbba", 2) == "abba")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("aabbaa", 1) == "aba")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("aaab", 2) == "aab")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("aaab", 1) == "ab")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("aabbaa", 1) == "aba")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("aabbaa", 2) == "aabbaa")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("aaab", 3) == "aaab")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("aaabbbb", 1) == "ab")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("aabbaaa", 2) == "aabbaa")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("ababababa", 2) == "ababababa")
    println("-" * 40)
    assert(Question.removeExtraConsecutives("aaaaaaaaa", 1) == "a")
    println("-" * 40)
  }
}