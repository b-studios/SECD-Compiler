package gcc

trait Interpreter
    extends Terms
    with feature.booleans.Interpreter
    with feature.integers.Interpreter
    with feature.pairs.Interpreter
    with feature.functions.Interpreter
    with feature.sideeffects.Interpreter {

  trait Value

  def interpret(term: Term): Value = (
    interpretBooleans orElse
    interpretIntegers orElse
    interpretPairs orElse
    interpretFunctions orElse
    interpretSideeffects
  )(term)


  def failWith(msg: String) = sys error msg.stripMargin
}