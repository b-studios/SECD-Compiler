package gcc
package feature
package booleans

import scala.language.implicitConversions

trait Syntax extends Terms { self:
      functions.Syntax with
      sideeffects.Syntax with
      integers.Syntax with Labeling =>

  implicit def liftBooleans(b: Boolean): Term = if (b) { 1 } else { 0 }

  def not(bool: Term) = bool < 1

  implicit class BooleanOps[T <% Term](a: T) {
    def ===(b: Term) = Eq(a, b)
    def =!=(b: Term) = not(a === b)
    def and(b: Term) = if_ (a) { b } else_ { false }
    def or(b: Term) = if_ (a) { true } else_ { b }
  }

  def if_(cond: Term)(thn: Term) = ProvideElse(els => IfThenElse(cond, thn, els))
  case class ProvideElse(buildIf: Term => Term) {
    def else_(els: Term): Term = buildIf(els)
    def else_if(elsCond: Term)(elsThn: Term) =
      ProvideElse(elsEls => buildIf(IfThenElse(elsCond, elsThn, elsEls)))
  }

  implicit def dontProvideElse(ifAThenB: ProvideElse): Term = ifAThenB else_ noop

  // syntax: switch(value)(branches*) withDefault (defaultValue)
  def switch(value: Term)(branches: (Term, Term)*) = WithDefault(value, branches)
  case class WithDefault(value: Term, branches: Seq[(Term, Term)]) {
    def withDefault(default: Term): Term = {
      val condName = freshLabel("switchCond").sym
      let(condName := value)(branches.foldRight(default) {
        case ((comp, body), els) => if_(condName === comp)(body) else_(els)
      })
    }
  }

  // for specifying switch cases
  implicit class CaseOps[T <% Term](t: T) {
    def ==>(body: Term) = (t, body)
  }
}