package plainer.actors

import akka.actor.ActorSystem  
import akka.actor.Props  
import akka.actor.actorRef2Scala  
import scala.util.Random
import akka.actor.{Actor,ActorLogging}
import plainer.actors.TeacherProtocol._

object TeacherProtocol{
  case class QuoteRequest()
  case class QuoteResponse(quoteString:String)
}

/*
 * Your Teacher Actor class. 
 * 
 * The class could use refinement by way of  
 * using ActorLogging which uses the EventBus of the Actor framework
 * instead of the plain old System out
 * 
 */

class TeacherActor extends Actor with ActorLogging {
  
  val quotes = List(  //send a message to the Teacher Actor
    "Moderation is for cowards",
    "Anything worth doing is worth overdoing",
    "The trouble is you think you have time",
    "You never gonna know if you never even try")

  def receive = {
    case QuoteRequest => {
      //Get a random Quote from the list and construct a response
      val quoteResponse = QuoteResponse(quotes(Random.nextInt(quotes.size)))
      log.info(quoteResponse.toString())
    }
  }
}


object MessageApp extends App{

  //Initialize the ActorSystem
  val actorSystem = ActorSystem("UniversityMessageSystem")

  //construct the Teacher Actor Ref
  val teacherActorRef = actorSystem.actorOf(Props[TeacherActor])

  //send a message to the Teacher Actor
  teacherActorRef ! QuoteRequest

  //Let's wait for a couple of seconds before we shut down the system
  Thread.sleep (2000) 

  //Shut down the ActorSystem.
  actorSystem.shutdown()

} 