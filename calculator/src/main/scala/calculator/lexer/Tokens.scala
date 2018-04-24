package calculator.lexer

import calculator.Positional

object Tokens {

  sealed trait TokenClass {
    def tokenClass: this.type = this // constructeur
  }

  sealed trait TokenInfo {
    def tokenClass: TokenClass  // constructeur
  }

  /** Tokens */
  case object BAD extends TokenInfo with TokenClass       // represents incorrect tokens.
  case object EOF extends TokenInfo with TokenClass       // represents end of file
  /** Insert other Tokens here */
  // Opérateurs
  case object EQSIGN extends TokenInfo with TokenClass    // =
  case object PLUS extends TokenInfo with TokenClass      // +
  case object MINUS extends TokenInfo with TokenClass     // -
  case object MULT extends TokenInfo with TokenClass      // *
  case object DIV extends TokenInfo with TokenClass       // /
  case object MODULO extends TokenInfo with TokenClass    // %
  case object POWER extends TokenInfo with TokenClass     // ^
  case object FACT extends TokenInfo with TokenClass      // factoriel
  case object GCD extends TokenInfo with TokenClass       // gcd
  case object INVMOD extends TokenInfo with TokenClass       // gcd
  case object SQRT extends TokenInfo with TokenClass      // racine carré

  // Contrôles
  case object LPAREN extends TokenInfo with TokenClass    // (
  case object RPAREN extends TokenInfo with TokenClass    // )
  case object VIRG extends TokenInfo with TokenClass      // ,

  // Token avec paramètre
  case class NUM (value:String) extends TokenInfo with TokenClass { // NUM(valeur)
    override def toString: String =  "NUM(" + value + ")"
  }
  case class ID(value:String) extends TokenInfo with TokenClass  {  // ID(valeur) variable
    override def toString: String = "ID(" + value + ")"
  }
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