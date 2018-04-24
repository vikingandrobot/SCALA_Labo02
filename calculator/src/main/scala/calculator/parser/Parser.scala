/**
  * File: Parser.scala
  * Modified by: Sathiya Kirushnapillai, Mathieu Monteverde, Michela Zucca
  * Date: 24 avril 2018
  */

package calculator.parser

import calculator.Main.memory
import calculator.lexer._
import scala.io.Source


/**
  * We completed this gile mainly by completing the parsing method for each
  * operations. The parsing methods are listed by priority order from less
  * to most.
  *
  * @param source the source to read from
  */
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

  /**
    * parse an expression. As the calculator is only supposed to process
    * on calculation and return the result we create a tree using the parseEquals
    * method and then expect a EOF Token. This prevents the user from
    * writing two calculation separated by spaces, for example 2+2    2+2
    *
    * @return the ExprTree representing the calculation
    */
  private def parseExpr: ExprTree = {
    val e = parseEquals
    eat(EOF)
    e
  }

  /**
    * This method was provided. It already assigns the value to the variable
    * in memory, so we didn't change that.
    *
    * @return the ExprTree representing the calculation
    */
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

  /**
    * Parses the addition and subtraction operations.
    * This method calls the parseMultDiv method. Then if it finds a plus operator
    * (or a minus operator), it will place the resulting ExprTree to the left of a
    * Plus instance (or a Minus instance) and call parseMultDiv to parse the right
    * side of the operator.
    *
    * @return an ExprTree representing the calculation
    */
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

  /**
    * Parses the multiplication and division operations.
    * This method calls the parseModulo method to compute the left tree and
    * then places it in a Mult or Div tree on the left. It then calls the parseModulo
    * method again to parse the right tree.
    *
    * @return an ExprTree representing the calculation
    */
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

  /**
    * Parse the modulo operator.
    * This method calls the parsePower method to find the left tree and
    * then calls the parsePower method again to find the right tree.
    *
    * @return an ExprTree representing the calculation
    */
  private def parseModulo: ExprTree = {
    var e = parsePower
    while (currentToken.info == MODULO) {
        eat(MODULO)
        e = Modulo(e, parsePower)
    }
    e
  }

  /**
    * Parses the power operation.
    * This method then calls the parseFactorial method to find the left tree and
    * the right tree.
    *
    * @return an ExprTree representing the calculation
    */
  private def parsePower: ExprTree = {
    var e = parseFactorial
    while (currentToken.info == POWER) {
      eat(POWER)
      e = Power(e, parseFactorial)
    }
    e
  }

  /**
    * Parse the factorail operator.
    * This method then calls the parseSimpleExpr to parse the simple expressions
    * in the calculation.
    *
    * @return an ExprTree representing the calculation
    */
  private def parseFactorial: ExprTree = {
    var e = parseSimpleExpr
    while (currentToken.info == FACT) {
      eat(FACT)
      e = Fact(e)
    }
    e
  }

  /**
    * Parses the simple expressions.
    * This method begins by detecting a minus unary negative operator
    * (this is useful for negative number and negative operation such as
    * -sqrt(4), -(2 + 3) etc.).
    *
    * Then it case matches the current token to call the correct parsing method.
    *
    * It then returns the result of said correct parsing method, wrapping the tree
    * in a UnaryMinus object if a minus sign was detected before.
    * @return
    */
  private def parseSimpleExpr: ExprTree = {

    /** Detect the presence of a minus sign **/
    var isMinus = false
    if (currentToken.info == MINUS) {
      eat(MINUS)
      isMinus = true
    }

    /** The tree we will be returning **/
    var e : ExprTree = null

    /** Parse each possible token */
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

    /** Wrap in a UnaryMinus if necessary **/
    if (isMinus) {
      e = UnaryMinus(e)
    }
    e
  }

  /**
    * Parses a numeric literal.
    * @param value the numeric literal String representation
    * @return a NumLit representing the numeric literal
    */
  private def parseNum(value: String): ExprTree = {
    eat(NUM(value))
    val e = NumLit(value)
    e
  }

  /**
    * Parses an identifier
    * @param value the identifier name as a String
    * @return an Identifier representing the variable
    */
  private def parseID(value: String): ExprTree = {
    eat(ID(value))
    val e = Identifier(value)
    e
  }

  /**
    * Parses a GCD function.
    * @return a Gcd representing the GCD function.
    */
  private def parseGCD(): ExprTree = {
    /** Read the gcd function and the first parenthesis */
    eat(GCD)
    eat(LPAREN)
    /** First argument **/
    val ret = parsePlusMinus
    /** Read the second argument */
    val e = Gcd(ret, parseVirgule())
    /** Read the closing parenthesis */
    eat(RPAREN)
    e
  }

  /**
    * Parses the modular inverse.
    * @return a InvMod representing the modular inverse
    */
  private def parseInvmod(): ExprTree = {
    /** Read the invmod function and the left parenthesis */
    eat(INVMOD)
    eat(LPAREN)
    /** First argument **/
    val ret = parsePlusMinus
    /** Read the second argument */
    val e = InvMod(ret, parseVirgule())
    /** Read the closing parenthesis */
    eat(RPAREN)
    e
  }

  /**
    * Parses a square root function. It the calls the parsePlusMinus
    * in order to parse any expression inside the square root.
    * @return a Sqrt object representing the square root
    */
  private def parseSQRT(): ExprTree = {
    eat(SQRT)
    val e = Sqrt(parsePlusMinus)
    e
  }

  /**
    * Parses a parenthesis. It the calls the parsePlusMinus
    * in order to parse any expression inside the parenthesis.
    * @return a ExprTree representing a calculation inside a parenthesis
    */
  private def parseParenthesis(): ExprTree = {
    eat(LPAREN)
    val ret = parsePlusMinus
    eat(RPAREN)
    ret
  }

  /**
    * Parses a coma.
    * @return an ExprTree representing the calculation after a coma.
    */
  private def parseVirgule(): ExprTree = {
    eat(VIRG)
    parsePlusMinus
  }
}

