/**
  * File: Calculator.scala
  * Modified by: Sathiya Kirushnapillai, Mathieu Monteverde, Michela Zucca
  * Date: 24 avril 2018
  */

package calculator

import scala.io.Source
import calculator.parser.Parser

class Calculator(source: Source) extends Parser (source:Source) {

  /**
    * Execute the calculator on source. THe method calls the computeSource method
    * and displays the result (memory updated or calculation result).
    *
    * If an error occurs such as the expression is not a legal expression, the Error
    * is caught and the message is displayed.
    */
  def execute(): Unit = {
    try {
      computeSource match {
        case Double.NegativeInfinity => println("Memory updated !")
        case result => println("Result : " + result)
      }
    } catch {
      case error: Error => println("Fatal error: " + error.getMessage)
    }
  }


}