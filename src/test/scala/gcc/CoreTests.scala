package gcc

import org.scalatest.FunSuite
import org.scalatest.Matchers.{ convertSymbolToHavePropertyMatcherGenerator => _, _}

import Predef.{ any2stringadd => _, _ }
import scala.language.{ implicitConversions, reflectiveCalls }

class CoreTests extends FunSuite with GCC {

  test("Should generate integer constants") {
    compile(3).code should equal (List(LDC(3), RTN))
  }

  test("Should generate integer addition correctly") {
    compile(term(3) + 5).code should equal (List(LDC(3), LDC(5), ADD, RTN))
    compile((term(3) + 5) + 6).code should equal (List(LDC(3), LDC(5), ADD, LDC(6), ADD, RTN))
    compile(term(3) + (term(5) + 6)).code should equal (List(LDC(3), LDC(5), LDC(6), ADD, ADD, RTN))
  }

  test("Should generate correct output for the other primitive operators") {
    compile((term(3) + 5) =!= 9).code should equal (List(LDC(1), LDC(3), LDC(5), ADD, LDC(9), CEQ, CGT, RTN))

    interpret((term(3) + 5) =!= 9) should equal (IntVal(1))
  }

  test("Should generate correct output for functions") {

      println(interpret {
        let(
          fun('abs)('x) { if_('x < 0) { 'x * (-1) } else_ { 'x } },

          fun('manhattan)('from, 'to) {
            ('abs('from.first - 'to.first) + 'abs('from.second - 'to.second))
          }
        )(debug('manhattan((1, 2), (5, 6))))
      })
  }

  test("Should interpret closures correctly") {

    interpret {
      let(
        'f := lam('x) { lam('y) { 'x + 'y }}
      )('f(1)(4))
    } should equal (IntVal(5))

  }
}
