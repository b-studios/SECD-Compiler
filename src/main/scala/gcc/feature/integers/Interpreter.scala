package gcc.feature
package integers

import scala.language.implicitConversions

trait Interpreter extends Terms { self: gcc.Interpreter =>

  case class IntVal(n: Int) extends Value
  implicit def intToVal(n: Int): Value = IntVal(n)
  implicit def boolToVal(b: Boolean): Value = IntVal(if (b) 1 else 0)

  def interpretIntegers: PartialFunction[Term, Value] = {
    case IntLit(n) => IntVal(n)
    case Add(l, r) => binOp(l, r, _ + _, "+")
    case Sub(l, r) => binOp(l, r, _ - _, "-")
    case Mul(l, r) => binOp(l, r, _ * _, "*")
    case Div(l, r) => binOp(l, r, _ / _, "/")

    case Eq(l, r)  => binOp(l, r, _ == _, "===")
    case Gt(l, r)  => binOp(l, r, _ > _,  ">")
    case Gte(l, r) => binOp(l, r, _ >= _, ">=")
  }

  private def binOp(lhs: Term, rhs: Term, op: (Int, Int) => Value, opName: String): Value
    = (interpret(lhs), interpret(rhs)) match {
      case (IntVal(x), IntVal(y)) => op(x, y)
      case (l, r) => this failWith(s"""Expects both sides of $opName to evaluate to integers.
                                      |  Lhs-Term: $lhs
                                      |  evaluated to $l
                                      |
                                      |  Rhs-Term: $rhs
                                      |  evaluated to $r""")
    }
}