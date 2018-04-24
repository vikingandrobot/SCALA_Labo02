/**
  * File: Calculator.scala
  * Modified by: Sathiya Kirushnapillai, Mathieu Monteverde, Michela Zucca
  * Date: 24 avril 2018
  */

package calculator

import scala.io.Source
import calculator.parser.Parser

class Calculator(source: Source) extends Parser (source:Source) {


  def execute(): Unit = {
    try {
      computeSource match {
        case Double.NegativeInfinity => println("Memory updated !")
        case result => println("Result : " + result)
      }
    } catch {
      case error: Error => println("Fatal error: " + error.getMessage)
    }

    //p.printTree
  }


}