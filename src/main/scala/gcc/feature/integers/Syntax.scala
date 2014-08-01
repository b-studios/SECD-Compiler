package gcc
package feature.integers

import scala.language.implicitConversions

trait Syntax extends Terms {

  implicit def liftIntegers(n: Int): Term = IntLit(n)
  implicit class IntOps[T <% Term](a: T) {
    def +(b: Term) = Add(a, b)
    def -(b: Term) = Sub(a, b)
    def *(b: Term) = Mul(a, b)
    def /(b: Term) = Div(a, b)

    def >=(b: Term) = Gte(a, b)
    def >(b: Term)  = Gt(a, b)
    def <=(b: Term) = b >= a
    def <(b: Term)  = b > a
  }

}