package gcc
package feature
package functions

import scala.language.implicitConversions

trait Syntax extends Terms { self: booleans.Syntax  =>

  // other syntax for functions
  def lam(params: Symbol*)(body: Term, defs: (Symbol, Term)*) =
    if (defs.size == 0)
      Lam(params.toList, body)
    else
      Lam(params.toList, letrec(defs: _*)(body))

  // creates a pair to be used immediately in letrecs
  def fun(name: Symbol)(params: Symbol*)(body: Term, defs: (Symbol, Term)*) = (name -> lam(params: _*)(body, defs: _*))

  def letrec(bindings: (Symbol, Term)*)(body: Term): Term = LetRec(bindings.toList, body)

  // We are defaulting to letrec since it is less instructions and is strictly more expressive - one should however still
  // use letrec when recursion is needed.
  def let(bindings: (Symbol, Term)*)(body: Term): Term =
    lam(bindings.head._1){
      if (bindings.size == 1) {
        body
      } else bindings.tail.foldRight(body) {
        case ((name, binding), acc) => lam(name)(acc)(binding)
      }
    }(bindings.head._2)


  implicit class ApplyOps[T <% Term](t: T) {
    def apply(args: Term*): Term = App(t, args.toList)
  }

  implicit class FunOps(p: (Symbol, Term)) {
    def onlyIf(preConds: Term*): (Symbol, Term) = p match {
      case (name, Lam(params, body)) => (name, Lam(params, if_(preConds.reduce(_ and _)) { body }))
      case _ => sys error s"Can only apply precondition to functions: $p"
    }
  }

  implicit class BindingOps(s: Symbol) {
    def :=(t: Term) = s -> t
  }

  implicit def liftSymbols(s: Symbol): Term = Var(s)
}