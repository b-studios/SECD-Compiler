package gcc
package feature
package derived

trait Lists extends Syntax { self: Labeling =>

  def empty: Term = 0

  implicit class ListOps[T <% Term](list: T) {
    def ::(el: Term) = cons(el, list)
    def head = list.first
    def tail = list.second
    def isEmpty = atom(list) and list === 0
  }

  def list(els: Term*) = els.foldRight(empty)(_ :: _)

}