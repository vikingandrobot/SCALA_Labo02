/**
  * File: Main.scala
  * Modified by: Sathiya Kirushnapillai, Mathieu Monteverde, Michela Zucca
  * Date: 24 avril 2018
  */

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

  def usage() : Unit = {
    println("======================================================")
    println("Calculator usage")
    println("======================================================")
    println("Arithmetic operations:")
    println("   + : Addition, for example: 1 + 2")
    println("   - : Subtraction, for example: 2 - 1")
    println("   * : Multiplication, for example: 2 * 4")
    println("   / : Division, for example: 4 / 2")
    println("")
    println("Other operations:")
    println("   % : Modulo, for example: 7 % 3")
    println("   ^ : Power, for example: 2^4")
    println("   ! : Factorial, for example: 5!")
    println("   - : Unary negative, for example:")
    println("       -1")
    println("       -(2 + 1)")
    println("       -x")
    println("       -gcd(3, 15)")
    println("       -sqrt(5)")
    println("       -3 + 2")
    println("       -(-(-1))")
    println("")
    println("Memory:")
    println("   = : Save value in memory variable, for example: x = 3")
    println("       To retrieve the value, type in the name of the saved variable.")
    println("       For example: 2 + x (will evaluate to 2 + 3)")
    println("")
    println("Parenthesis:")
    println("   You can use parenthesis, for example: 6 * (2 + 3)")
    println("")
    println("Functions:")
    println("   gcd(x, y) : Greatest common divisor, for example: gcd(4, 8)")
    println("   sqrt(x, y) : Square root, for example: sqrt(4)")
    println("")
    println("Notes:")
    println("   This calculator doesn't support decimal numbers in calculations. Sorry.")
  }
}