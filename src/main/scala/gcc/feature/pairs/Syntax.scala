package gcc
package feature
package pairs

import scala.language.implicitConversions

trait Syntax extends Terms {

  def cons(head: Term, tail: Term) = Cons(head, tail)
  def atom(p: Term) = Atom(p)

  implicit class PairOps[T <% Term](pair: T) {
    def first = Car(pair)
    def second = Cdr(pair)
  }

  implicit def pairToCons[A <% Term, B <% Term](scalaPair: (A, B)): Term = cons(scalaPair._1, scalaPair._2)
}