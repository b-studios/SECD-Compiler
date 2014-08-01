package gcc
package feature.pairs

trait Terms extends gcc.Terms {
  case class Cons(head: Term, tail: Term) extends Term
  case class Car(pair: Term) extends Term
  case class Cdr(pair: Term) extends Term
  case class Atom(t: Term) extends Term
}