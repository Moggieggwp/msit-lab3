import WumpusWorld.{checkRoom, _}
import akka.actor._

import scala.util.Random

object WumpusWorld {
  sealed trait Room
  case object Empty extends Room
  case object WumpusInside extends Room
  case object KillWumpus extends Room
  case object ShotInside extends Room

  sealed trait GameState
  case object GameOver extends GameState

  case class GameRunning(shots: Int,room: Room) extends GameState {
    if (room == Empty) {
      println("Room is empty")
    }
    else if (room == WumpusInside) {
      println("There is Wumpus inside. You died!")
      System.exit(0)
    }
    else if (room == KillWumpus) {
      println("Shot")
      println("Shot count: " + shots)
      println("Wumpus was killed. You is winner!")
      System.exit(0)
    }
    else if (room == ShotInside) {
      println("Shot")
      println("Shot count: " + shots)

      if (shots < 1) {
        println("You have not shots. Game Over")
        System.exit(0)
      }
    }
  }


  def checkRoom(game: GameRunning): GameState = game.room match {
    case Empty => game
    case WumpusInside => GameOver
    case ShotInside => game.copy(room = (Empty))
  }
}

class Engine extends Actor {
  var shots = 5

  def state(room: Room): GameRunning = GameRunning(shots, room)

  def getRandomAction() {

    val x = Random.between(0, 3)
    if (x == 0) checkRoom(state(WumpusInside))
    else if (x == 1) {
      shots -= 1
      checkRoom(state(ShotInside))
    }
    else if (x == 2) {
      shots -= 1
      checkRoom(state(ShotInside))
    }
    else if (x > 2) checkRoom(state(Empty))
  }

  override def receive: Receive = {
    case _ => getRandomAction
  }
}

object StartGame extends App {
  val system = ActorSystem("WumpusActor")
  val wumpusActor = system.actorOf(Props[Engine], name = "WumpusActor")
  for (x <- 0 until 1000) {
    wumpusActor ! Empty
    wumpusActor ! WumpusInside
    wumpusActor ! KillWumpus
    wumpusActor ! ShotInside
  }
}
