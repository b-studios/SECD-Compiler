package gcc

import org.scalatest.FunSuite
import org.scalatest.Matchers.{ convertSymbolToHavePropertyMatcherGenerator => _, _}

import Predef.{ any2stringadd => _, _ }
import scala.language.{ implicitConversions, reflectiveCalls }

class CollectionTests extends FunSuite with GCC with programs.Collections {

  test("Should generate integer constants") {
     compile(
       let(
        'id  := lam('n) { 'n },
        'cmp := lam('x, 'y) { 'y - 'x },
        collections.TreeMap,
        'map := 'new_TreeMap(empty, 'cmp)
       ){

         'map.call('TreeMap, 'put)(42, 18) ~:
         'map.call('TreeMap, 'put)(12, 4)  ~:
         'map.call('TreeMap, 'put)(34, 4)  ~:
         debug('map.call('TreeMap, 'print)())
       }).showRaw
  }

   test("Should generate correct tuple access") {
     compile((1, 2, 3).at(1, 3)).code should be (List(LDC(1), LDC(2), LDC(3), CONS, CONS, CAR, RTN))
     compile((1, 2, 3).at(2, 3)).code should be (List(LDC(1), LDC(2), LDC(3), CONS, CONS, CDR, CAR, RTN))
     compile((1, 2, 3).at(3, 3)).code should be (List(LDC(1), LDC(2), LDC(3), CONS, CONS, CDR, CDR, RTN))

     compile((1, 2, 3).bind('a, 'b, 'c) { debug('a) }).showRaw
  }
}