package plainer.external

import akka.actor.{Actor, ActorSystem}
import akka.actor.Props
import akka.event.Logging

class MyActor extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case "test" => log.info("received test")
    case _ => log.info("received unknown message")
  }
}

class SimpleCounter extends Actor {
  var count = 0
  def receive = {
    case "add" => count += 1
    case "echo" => println(count)
    case _     => // do nothing
  }
}

object TryActors extends App {
  def doStuff = {
    val system = ActorSystem("system")
    val counter = system.actorOf(Props(new SimpleCounter), "counter")
    
    counter ! "add"
    counter ! "add"
    counter ! "add"
    counter ! "echo" // 3
  }
  doStuff
}

