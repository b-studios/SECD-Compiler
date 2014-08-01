package gcc
package feature.sideeffects

trait Terms extends gcc.Terms {

  trait SideEffect extends Term

  case class Debug(value: Term) extends SideEffect
  case object Noop extends SideEffect
  case class Sequence(first: Term, thn: Term) extends SideEffect
  case class Assign(variable: Symbol, value: Term) extends SideEffect
}