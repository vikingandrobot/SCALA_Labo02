package calculator.lexer

import calculator.Positional

object Tokens {

  sealed trait TokenClass {
    def tokenClass: this.type = this
  }

  sealed trait TokenInfo {
    def tokenClass: TokenClass
  }

  /** Tokens */
  case object BAD extends TokenInfo with TokenClass       // represents incorrect tokens.
  case object EOF extends TokenInfo with TokenClass       // represents end of file
  /** Insert other Tokens here */
  case object EQSIGN extends TokenInfo with TokenClass    // =
  case object PLUS extends TokenInfo with TokenClass      // +
  case object MINUS extends TokenInfo with TokenClass     // -
  case object LPAREN extends TokenInfo with TokenClass     // (
  case object RPAREN extends TokenInfo with TokenClass     // )
  ???

  /** Token */
  class Token(val info: TokenInfo) extends Positional {
    override def toString: String = info.toString
    def tokenClass: TokenClass = info.tokenClass
  }

  object Token {
    def apply(info: TokenInfo): Token = new Token(info)
    def unapply(token: Token): Option[TokenInfo] = Some(token.info)
  }
}