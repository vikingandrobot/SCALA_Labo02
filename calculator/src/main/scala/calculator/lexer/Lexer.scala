/**
  * File: Lexer.scala
  * Modified by: Sathiya Kirushnapillai, Mathieu Monteverde, Michela Zucca
  * Date: 24 avril 2018
  */

package calculator.lexer

import scala.io.Source

/**
  * We completed this file mainly by adding the possible patterns to the case match
  * inside the nextToken method.
  *
  * @param source the source to read from
  */
class Lexer (source:Source) {

  import Tokens._

  var position: Int = 0
  var ch: Char = ' '
  var eof: Boolean = false

  val numeric: List[Char] = ('0' to '9').toList
  val alphabetic: List[Char] = ('a' to 'z').toList ++ ('A' to 'Z').toList
  val alphanumeric: List[Char] = numeric ++ alphabetic ++ List('_')

  /** Works like an iterator, and returns the next token from the input stream. */
  def nextToken: Token = {
    if (eof) {
      position = source.pos
      setToken(EOF)
    } else {
      if (position == 0) nextChar
      position = source.pos

      /**
        * Here we match the current character with different pattern cases.
        *
        * You'll notice that the 'case x ...' and 'case y ...' used to detect
        * the numbers and the keywords or variables don't use the method
        * setToken. That is because the method readMultiple used to
        * read said tokens finishes by calling nextChar, thus placing
        * a character that does not belong to the current token being read inside
        * the ch variable. We cannot call nextToken then because it will
        * skip said character due to the fact that nextToken calls nextChar one to many time in
        * that case.
        */
      ch match {
        case ' ' => skipToken
        case '(' => setToken(LPAREN)
        case ')' => setToken(RPAREN)
        case ',' => setToken(VIRG)
        case '=' => setToken(EQSIGN)
        case '+' => setToken(PLUS)
        case '-' => setToken(MINUS)
        case '*' => setToken(MULT)
        case '/' => setToken(DIV)
        case '%' => setToken(MODULO)
        case '^' => setToken(POWER)
        case '!' => setToken(FACT)
        case '0' => setToken(NUM(ch.toString))
        case x if numeric.contains(x) => Token(NUM(readMultiple(numeric))).setPos(position)
        case y if alphabetic.contains(y) => Token(keywordOrId(readMultiple(alphanumeric))).setPos(position)
        case _ => fatalError("Token doesn't exist")    // n'existe pas
      }
    }
  }

  /** Checks and set if the multiple Char found is a keyword or a variable */
  def keywordOrId(str: String): TokenInfo = {
    str.toLowerCase match {
      case "gcd" => GCD
      case "sqrt" => SQRT
      case "invmod" => INVMOD
      case _ => ID(str)
    }
  }

  /** Moves the iterator to the next Char of the input source */
  def nextChar: Unit = if (source.hasNext) ch = source.next() else { ch = ' '; eof = true }

  /** Moves the iterator to the next Char and set previous Token */
  def setToken(tkn: TokenInfo): Token = { nextChar; Token(tkn).setPos(position) }

  /** Moves the iterator to the next Char and skip the current token, useful for empty Char */
  def skipToken: Token = { nextChar; nextToken }

  /** Reads multiple Char at once, useful for detecting variables and keywords */
  def readMultiple(allowed: List[Char]): String = {
    var str = "" + ch
    nextChar
    while (allowed.contains(ch) && !eof) {
      str += ch
      nextChar
    }
    str
  }

  /** Modified to throw an Error containing a message indicating the source of the error */
  def fatalError(msg: String): Nothing = {
    throw new Error(msg)
  }
}