package calculator.parser

object Trees {

  sealed trait ExprTree {
    @throws(classOf[Exception])
    def compute: Double = this match {
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


  /** Leaves Expression Trees */
  case class Empty() extends ExprTree {
    override def toString: String = "Empty('')"
  }
  case class NumLit(value: String) extends ExprTree
  case class Identifier(value: String) extends ExprTree {
    override def toString: String = "Identifier('" + value + "')"
  }
}