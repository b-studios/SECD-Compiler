package gcc
package feature.functions

trait Terms extends gcc.Terms {
  case class App(fun: Term, args: List[Term]) extends Term
  case class Lam(args: List[Symbol], body: Term) extends Term
  case class Var(sym: Symbol) extends Term
  case class LetRec(bindings: List[(Symbol, Term)], body: Term) extends Term
}