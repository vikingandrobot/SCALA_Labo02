package calculator.lexer

import scala.io.Source

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

  def fatalError(msg: String): Nothing = {
    throw new Error(msg)
  }
}