package gcc
package feature
package derived

trait Trees extends Syntax { self: Labeling with DerivedSyntax  =>

  def node(lhs: Term, value: Term, rhs: Term) = (lhs, value, rhs)
  def leaf(value: Term) = (0, value, 0)

  implicit class TreeOps[T <% Term](tree: T) {
    def leftTree  = tree.first
    def rightTree = tree.second.second
    def nodeValue = tree.second.first
    def isLeaf    = not(tree.isEmpty) and tree.leftTree === 0 and tree.rightTree === 0
  }
}