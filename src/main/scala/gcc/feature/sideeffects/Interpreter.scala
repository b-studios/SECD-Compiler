package gcc.feature
package sideeffects

import scala.language.implicitConversions

trait Interpreter extends Terms { self: gcc.Interpreter with functions.Interpreter =>

  case object UnitValue extends Value

  def interpretSideeffects: PartialFunction[Term, Value] = {
    case Sequence(fst, thn) => { interpret(fst); interpret(thn) }
    case Debug(term)        => { println(interpret(term)); UnitValue }
    case Noop               => UnitValue
    case Assign(v, term)    => { environment.value.update(v, interpret(term)); UnitValue }
  }
}