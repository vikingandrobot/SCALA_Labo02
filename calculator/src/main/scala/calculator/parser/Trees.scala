package calculator.parser

import calculator.Main.memory
import calculator.utils.op
import calculator.utils.gcd
import calculator.utils.sqrt
import calculator.utils.factorial

object Trees {

  sealed trait ExprTree {
    @throws(classOf[Exception])
    def compute: Double = this match {
      case Assign(l, r) => val value = r.compute; memory += (l.value -> value); Double.NegativeInfinity
      case Plus(l, r) => op('+', l.compute, r.compute)
      case Minus(l, r) => op('-', l.compute, r.compute)
      case Mult(l, r) => op('*', l.compute, r.compute)
      case Div(l, r) => op('/', l.compute, r.compute)
      case Modulo(l, r) => op('%', l.compute, r.compute)
      case Power(l, r) => op('^', l.compute, r.compute)
      case Gcd(l, r) => gcd(l.compute.toInt, r.compute.toInt)
      case Fact(l, Empty()) => factorial(l.compute.toInt)
      case Sqrt(l, Empty()) => sqrt(l.compute)
      case UnaryMinus(l, Empty()) => -(l.compute)
      case NumLit(x) => x.toDouble
      case Identifier(id) => memory(id)
      case _ => ???
    }
  }

  /** Nodes Expression Trees */
  /** lhs: left hand side, rhs: right hand side */
  case class Assign(ident: Identifier, value: ExprTree) extends ExprTree
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Mult(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Modulo(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Power(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Gcd(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Fact(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Sqrt(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class UnaryMinus(lhs: ExprTree, rhs: ExprTree) extends ExprTree


  /** Leaves Expression Trees */
  case class Empty() extends ExprTree {
    override def toString: String = "Empty('')"
  }
  case class NumLit(value: String) extends ExprTree
  case class Identifier(value: String) extends ExprTree {
    override def toString: String = "Identifier('" + value + "')"
  }
}