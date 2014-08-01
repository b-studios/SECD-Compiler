package gcc
package feature.integers

trait Terms extends gcc.Terms {

  case class IntLit(n: Int) extends Literal

  trait IntOp extends Term
  case class Add(l: Term, r: Term) extends IntOp
  case class Sub(l: Term, r: Term) extends IntOp
  case class Mul(l: Term, r: Term) extends IntOp
  case class Div(l: Term, r: Term) extends IntOp

  trait IntCompOp extends IntOp
  case class Eq(l: Term, r: Term) extends IntCompOp
  case class Gt(l: Term, r: Term) extends IntCompOp
  case class Gte(l: Term, r: Term) extends IntCompOp

}