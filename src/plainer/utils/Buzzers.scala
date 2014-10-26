/**
 *
 */


package plainer.utils

/**
 * @author eddie
 *
 */
package Buzzers {

  import plainer.utils.IntExtended
  
  abstract class Buzzer {
    def run: Unit
  }
  
  object FizzBuzzEasy extends Buzzer {
    
    def run : Unit = {
      1 to 100 foreach { n =>
        println((n % 3, n % 5) match {
          case (0, 0) => "FizzBuzz"
          case (0, _) => "Fizz"
          case (_, 0) => "Buzz"
          case _ => n
        })
      }
    }
  }

  object FizzBuzzMedium extends Buzzer {
    
    def run : Unit = {
      1 to 100 map doStuff foreach println
    }
    
    def doStuff = {
      def replaceMultiples(x: Int, rs: (Int, String)*): Either[Int, String] =
        rs map { 
          case (n, s) => Either cond(x % n == 0, s, x)
        } reduceLeft ((a, b) => 
          a fold(_ => b, s => b fold(_ => a, t => Right(s + t))))
     
      replaceMultiples(_: Int, 3 -> "Fizz", 5 -> "Buzz") fold(_.toString, identity)
    }
  }
 
  /*
   *  Usage:
   *  for (i <- 1 to 100) 
   *    println
   *    f(i, 15, "FizzBuzz", 
   *      f(i, 3, "Fizz", 
   *        f(i, 5, "Buzz", i.toString))))
   */
  object FizzBuzzHard extends Buzzer {
    implicit def intToInt(i: Int) = new IntExtended(i)

    def func(
      number: Int, 
      divisor: Int, 
      met: String, 
      notMet: String
    ): String = { 
      if (number isModuleOf divisor) 
        met 
      else 
        notMet
    }
        
    def run : Unit = {
      for (i <- 1 to 100) {
        println(
          func(i, 15, "FizzBuzz",
            func(i, 3, "Fizz",
              func(i, 5, "Buzz", i.toString)
            )
          )
        )
      }
    }
  }
  

}