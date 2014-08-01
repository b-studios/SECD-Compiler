package gcc.feature
package booleans

import scala.language.implicitConversions

trait Interpreter extends Terms { self: gcc.Interpreter with integers.Interpreter =>

  object BoolVal {
    def unapply(b: Value): Option[Boolean] = b match {
      case IntVal(1) => Some(true)
      case IntVal(0) => Some(false)
      case _ => None
    }
  }

  def interpretBooleans: PartialFunction[Term, Value] = {
    case IfThenElse(cond, thn, els) => interpret(cond) match {

      case BoolVal(true)  => interpret(thn)
      case BoolVal(false) => interpret(els)

      case c => this failWith s"""Conditional requires condition to evaluate to a truth value:
                                 |  Term: $cond
                                 |  evaluated to: $c"""
    }
  }
}