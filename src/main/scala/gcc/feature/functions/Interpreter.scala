package gcc.feature
package functions

import scala.util.DynamicVariable
import scala.collection.mutable

/**
 * This interpreter implementation is a playful experiment with
 * Scala's DynamicVariable
 */
trait Interpreter extends Terms { self: gcc.Interpreter =>

  case class Record(bindings: mutable.Map[Symbol, Value], parent: Option[Record]) {
    def lookup(sym: Symbol): Value = bindingFrameFor(sym).bindings(sym)

    def update(sym: Symbol, value: Value): Unit =
      bindingFrameFor(sym).bindings.update(sym, value)

    private def bindingFrameFor(sym: Symbol): Record =
      if (bindings isDefinedAt sym)
        this
      else
        parent
          .map(_.bindingFrameFor(sym))
          .getOrElse(self failWith s"Unbound variable: ${sym.name}")

  }
  object GlobalRecord extends Record(mutable.Map.empty, None)

  val environment = new DynamicVariable[Record](GlobalRecord)

  case class Closure(params: List[Symbol], env: Record, body: Term) extends Value
  case object Dummy extends Value

  def interpretFunctions: PartialFunction[Term, Value] = {
    case a @ App(fun, args) => interpret(fun) match {
      case Closure(params, env, body) if (params.size != args.size) =>
        this failWith s"""Failure: ${params.size}-ary function called with ${args.size} arguments.
                         |  $a"""
      case Closure(params, env, body) =>
        val activationRecord: Record =
          Record(mutable.Map(params zip args.map(interpret _): _*), Some(env))
        environment.withValue(activationRecord){
          interpret(body)
        }
      case o => this failWith s"""Unexpected value in function position: $a"""
    }

    case Var(name) => environment.value lookup name

    case Lam(variables, body) => Closure(variables, environment.value, body)

    case LetRec(bindings, body) =>
      val dummies = bindings.map {
        case (name, _) => (name -> Dummy)
      }

      val dummyFrame = Record(mutable.Map(dummies: _*), Some(environment.value))

      environment.withValue(dummyFrame) {
        bindings.foreach {
          case (name, term) => dummyFrame.bindings.put(name, interpret(term))
        }
        interpret(body)
      }
  }
}