package calculator

import scala.io.Source
import calculator.parser.Parser

class Calculator(source: Source) extends Parser (source:Source) {


  def execute(): Unit = {
    val p = new Parser(source)
    try {
      computeSource match {
        case Double.NegativeInfinity => println("Memory updated !")
        case result => println("Result : " + result)
      }
    } catch {
      case error: Error => println(error.getMessage)
    }

    //p.printTree
  }


}