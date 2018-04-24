/**
  * File: Trees.scala
  * Modified by: Sathiya Kirushnapillai, Mathieu Monteverde, Michela Zucca
  * Date: 24 avril 2018
  */

package calculator.parser

import calculator.Main.memory
import calculator.utils.op
import calculator.utils.gcd
import calculator.utils.sqrt
import calculator.utils.factorial
import calculator.utils.modInvert

/**
  * We completed this file by adding the declarations for the ExprTree
  * case classes.
  *
  * The majority has a left tree child and a right tree child. Others
  * such as Sqrt (square root) or Fact (factorial) only have one operand
  * and can contain a sub tree as a child.
  *
  * We tried reusing the functions defined in the previous practical work
  * as much as possible. This allowed us to rely on exceptions thrown
  * by the utility functions to detect errors (such as negative square roots).
  *
  * The only exception is the usage of the memory which is stored in the Calculator
  * class. We decided to handle the possible exception of noSuchElementException
  * by throwing an error to display higher in the stack.
  */
object Trees {

  sealed trait ExprTree {
    @throws(classOf[Exception])
    def compute: Double = this match {
      case Assign(l, r) => Double.NegativeInfinity
      case Plus(l, r) => op('+', l.compute, r.compute)
      case Minus(l, r) => op('-', l.compute, r.compute)
      case Mult(l, r) => op('*', l.compute, r.compute)
      case Div(l, r) => op('/', l.compute, r.compute)
      case Modulo(l, r) => op('%', l.compute, r.compute)
      case Power(l, r) => op('^', l.compute, r.compute)
      case Gcd(l, r) => gcd(l.compute.toInt, r.compute.toInt)
      case InvMod(l, r) => modInvert(l.compute.toInt, r.compute.toInt)
      case Fact(c) => factorial(c.compute.toInt)
      case Sqrt(c) => sqrt(c.compute)
      case UnaryMinus(c) => -(c.compute)
      case NumLit(x) => x.toDouble
      case Identifier(id) => try {
        memory(id)
      } catch {
        case ex: java.util.NoSuchElementException => throw new Error("Variable doesn't exist in memory.")
      }
      case _ => throw new Error("Unknown expression.")
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
  case class InvMod(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Fact(child: ExprTree) extends ExprTree
  case class Sqrt(child: ExprTree) extends ExprTree
  case class UnaryMinus(child: ExprTree) extends ExprTree


  /** Leaves Expression Trees */
  case class NumLit(value: String) extends ExprTree
  case class Identifier(value: String) extends ExprTree {
    override def toString: String = "Identifier('" + value + "')"
  }
}