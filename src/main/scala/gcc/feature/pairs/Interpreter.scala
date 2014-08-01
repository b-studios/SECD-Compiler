package gcc.feature
package pairs

import scala.language.implicitConversions

trait Interpreter extends Terms { self: gcc.Interpreter =>

  case class PairVal(car: Value, cdr: Value) extends Value

  def interpretPairs: PartialFunction[Term, Value] = {
    case Cons(head, tail) => PairVal(interpret(head), interpret(tail))
    case Car(p)  => assertPair(p, "first")._1
    case Cdr(p)  => assertPair(p, "second")._2
    case Atom(p) => interpret(p) match {
      case i: IntVal => IntVal(1)
      case o => IntVal(0)
    }
  }

  def assertPair(p: Term, proj: String) = interpret(p) match {
    case PairVal(car, cdr) => (car, cdr)
    case o => this failWith s"Can only project $proj of pairs, but got $o"
  }
}