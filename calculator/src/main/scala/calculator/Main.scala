package calculator

import scala.io.Source
import scala.io.StdIn.readLine

object Main {
  var memory: Map[String, Double] = Map()

  def main(args: Array[String]): Unit = {
    println("Welcome to the Scala calculator !")
    console()
  }

  def console(): Unit = {
    readLine match {
      case "quit" => println("Bye !")
      case "usage" => usage(); console()
      case s => new Calculator(Source.fromString(s)).execute(); console()
    }
  }

  def usage() = ???
}