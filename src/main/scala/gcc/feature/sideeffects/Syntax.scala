package gcc
package feature
package sideeffects

import scala.language.implicitConversions

trait Syntax extends Terms {

  implicit class SeqOps[T <% Term](thn: T) {
    def ~:(fst: Term) = Sequence(fst, thn)
  }

  implicit class AssignOps(s: Symbol) {
    def <~(term: Term) = Assign(s, term)
  }

  def debug[T <% Term](t: T): Term = Debug(t)
  def noop = Noop
}