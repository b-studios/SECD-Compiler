package gcc

trait Terms {
  trait Term
  trait Literal extends Term
}

trait Syntax
    extends Terms
    with feature.booleans.Syntax
    with feature.integers.Syntax
    with feature.functions.Syntax
    with feature.pairs.Syntax
    with feature.sideeffects.Syntax { self: Labeling =>

  def term(t: Term): Term = t

}

trait DerivedSyntax
    extends Syntax
    with feature.derived.Products
    with feature.derived.Classes
    with feature.derived.Lists
    with feature.derived.Trees { self: Labeling => }