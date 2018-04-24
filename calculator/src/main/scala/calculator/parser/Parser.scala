/**
  * File: Parser.scala
  * Modified by: Sathiya Kirushnapillai, Mathieu Monteverde, Michela Zucca
  * Date: 24 avril 2018
  */

package calculator.parser

import calculator.Main.memory
import calculator.lexer._
import scala.io.Source

class Parser(source:Source) extends Lexer(source:Source) {


  import Trees._
  import calculator.lexer.Tokens._

  def computeSource: Double = { readToken; parseExpr.compute }

  def printTree: Unit = { readToken; println(parseExpr) }

  /** Store the current token, as read from the lexer. */
  private var currentToken: Token = Token(BAD)

  /** update currentToken using nextToken in the Lexer. */
  def readToken: Unit = { currentToken = nextToken }

  /** ""Eats"" the expected token, or terminates with an error. */
  private def eat(tokenClass: TokenClass): Unit = if (tokenClass == currentToken.info.tokenClass) readToken else expected(tokenClass)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  private def expected(tokenClass: TokenClass, more: TokenClass*): Nothing = fatalError("expected: " + (tokenClass :: more.toList).mkString(" or ") + ", found: " + currentToken)

  private def parseExpr: ExprTree = {
    val e = parseEquals
    eat(EOF)
    e
  }

  private def parseEquals: ExprTree = {
    val e = parsePlusMinus
    if (currentToken.info == EQSIGN) {
      eat(EQSIGN)
      e match {
        case id @ Identifier(_) => {
          val rhs = parseEquals
          rhs match {
            case Assign(_, _) => fatalError("Invalid variable declaration !")
            case _ => {
              memory += (id.value -> rhs.compute)
              Assign(id, rhs)
            }
          }
        }
        case _ => fatalError("Invalid variable declaration !")
      }
    } else {
      e
    }
  }

  private def parsePlusMinus: ExprTree = {
    var e = parseMultDiv
    while (currentToken.info == PLUS || currentToken.info == MINUS) {
      if (currentToken.info == PLUS) {
        eat(PLUS)
        e = Plus(e, parseMultDiv)
      } else {
        eat(MINUS)
        e = Minus(e, parseMultDiv)
      }
    }
    e
  }

  // Implement the other grammar methods
  private def parseMultDiv: ExprTree = {
    var e = parseModulo
    while (currentToken.info == MULT || currentToken.info == DIV) {
      if (currentToken.info == MULT) {
        eat(MULT)
        e = Mult(e, parseModulo)
      } else {
        eat(DIV)
        e = Div(e, parseModulo)
      }
    }
    e
  }

  private def parseModulo: ExprTree = {
    var e = parsePower
    while (currentToken.info == MODULO) {
        eat(MODULO)
        e = Modulo(e, parsePower)
    }
    e
  }

  private def parsePower: ExprTree = {
    var e = parseFactorial
    while (currentToken.info == POWER) {
      eat(POWER)
      e = Power(e, parseFactorial)
    }
    e
  }

  private def parseFactorial: ExprTree = {
    var e = parseSimpleExpr
    while (currentToken.info == FACT) {
      eat(FACT)
      e = Fact(e)
    }
    e
  }

  private def parseSimpleExpr: ExprTree = {
    var isMinus = false
    if (currentToken.info == MINUS) {
      eat(MINUS)
      isMinus = true
    }

    var e : ExprTree = null

    // Here you want to match simple expressions such as NUM(value) and parse them (for example with the parseExprTreeToken method).
    currentToken.info match {
      case LPAREN => e = parseParenthesis // Parenthesis
      case VIRG => e = parseVirgule // Virgule
      case NUM(x) => e = parseNum(x)
      case ID(x) => e = parseID(x)
      case GCD => e = parseGCD
      case INVMOD => e = parseInvmod
      case SQRT => e = parseSQRT
      case _    => fatalError("Invalid token " + currentToken.toString)
    }

    if (isMinus) {
      e = UnaryMinus(e)
    }
    e
  }

  private def parseExprTreeToken[T <: ExprTree](retTree: T): ExprTree = {
    readToken
    retTree
  }

  private def parseNum(value: String): ExprTree = {
    eat(NUM(value))
    val e = NumLit(value)
    e
  }

  private def parseID(value: String): ExprTree = {
    eat(ID(value))
    val e = Identifier(value)
    e
  }

  private def parseGCD(): ExprTree = {
    eat(GCD)
    eat(LPAREN)
    val ret = parsePlusMinus
    eat(VIRG)
    val e = Gcd(ret, parsePlusMinus)
    eat(RPAREN)
    e
  }

  private def parseInvmod(): ExprTree = {
    eat(INVMOD)
    eat(LPAREN)
    val ret = parsePlusMinus
    eat(VIRG)
    val e = InvMod(ret, parsePlusMinus)
    eat(RPAREN)
    e
  }

  private def parseSQRT(): ExprTree = {
    eat(SQRT)
    val e = Sqrt(parsePlusMinus)
    e
  }

  private def parseParenthesis(): ExprTree = {
    eat(LPAREN)
    val ret = parsePlusMinus
    eat(RPAREN)
    ret
  }

  private def parseVirgule(): ExprTree = {
    eat(VIRG)
    parsePlusMinus
  }
}

