package gcc
package feature.booleans

trait Terms extends gcc.Terms {
  case class IfThenElse(cond: Term, thn: Term, els: Term) extends Term
}