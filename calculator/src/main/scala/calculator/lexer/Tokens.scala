/**
  * File: Tokens.scala
  * Modified by: Sathiya Kirushnapillai, Mathieu Monteverde, Michela Zucca
  * Date: 24 avril 2018
  */

package calculator.lexer

import calculator.Positional

/**
  * We completed this file by adding the declarations for the possible
  * types of Tokens.
  *
  * We used case objects when possible and case class for Token
  * that need parameters to mean something (numeric literals and identifiers).
  */
object Tokens {

  sealed trait TokenClass {
    def tokenClass: this.type = this // Constructor
  }

  sealed trait TokenInfo {
    def tokenClass: TokenClass  // Constructor
  }

  /** Tokens */
  case object BAD extends TokenInfo with TokenClass       // represents incorrect tokens.
  case object EOF extends TokenInfo with TokenClass       // represents end of file
  /** Insert other Tokens here */

  /** Operators **/
  case object EQSIGN extends TokenInfo with TokenClass    // =
  case object PLUS extends TokenInfo with TokenClass      // +
  case object MINUS extends TokenInfo with TokenClass     // -
  case object MULT extends TokenInfo with TokenClass      // *
  case object DIV extends TokenInfo with TokenClass       // /
  case object MODULO extends TokenInfo with TokenClass    // %
  case object POWER extends TokenInfo with TokenClass     // ^
  case object FACT extends TokenInfo with TokenClass      // factorial
  case object GCD extends TokenInfo with TokenClass       // Greatest common divisor
  case object INVMOD extends TokenInfo with TokenClass    // Modular invese
  case object SQRT extends TokenInfo with TokenClass      // Square root

  /** Controls **/
  case object LPAREN extends TokenInfo with TokenClass    // (
  case object RPAREN extends TokenInfo with TokenClass    // )
  case object VIRG extends TokenInfo with TokenClass      // ,

  /** Token with parameters **/
  /** Numeric literals **/
  case class NUM (value:String) extends TokenInfo with TokenClass {
    override def toString: String =  "NUM(" + value + ")"
  }
  /** Identifiers **/
  case class ID(value:String) extends TokenInfo with TokenClass  {
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