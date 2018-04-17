package calculator.lexer

import calculator.Calculator
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
        case '(' => setToken(Tokens.LPAREN)
        case ')' => setToken(Tokens.RPAREN)
        case ',' => setToken(Tokens.VIRG)
        case '=' => setToken(Tokens.EQSIGN)
        case '+' => setToken(Tokens.PLUS)
        case '-' => setToken(Tokens.MINUS)
        case '*' => setToken(Tokens.MULT)
        case '/' => setToken(Tokens.DIV)
        case '%' => setToken(Tokens.MODULO)
        case '^' => setToken(Tokens.POWER)
        case '!' => setToken(Tokens.FACT)
        case 'g' => setToken(Tokens.GCD)
        case 's' => setToken(Tokens.SQRT)
        // todo passer le paramètre de numérique
        case x if(numeric.contains(x)) => setToken(Tokens.NUM)
        // todo vérifier si c'est une variable et ajouter la valeur...
       // case y if(keywordOrId(y)) => setToken(Tokens.ID)
        case _ => setToken(Tokens.BAD)    // mauvais
      }
    }
  }

  /** Checks and set if the multiple Char found is a keyword or a variable */
  def keywordOrId(str: String): TokenInfo = {
    str.toLowerCase match {
      case _ => ???
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
    println("Fatal error", msg)
    sys.exit(1)
  }
}