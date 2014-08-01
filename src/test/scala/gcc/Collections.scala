package gcc

import org.scalatest.FunSuite
import org.scalatest.Matchers.{ convertSymbolToHavePropertyMatcherGenerator => _, _}

import Predef.{ any2stringadd => _, _ }
import scala.language.{ implicitConversions, reflectiveCalls }

class CollectionTests extends FunSuite with GCC with programs.Collections  with programs.GameObjects {

  test("game map cells") {
    compile(
       let(
        gameObjects.Cell,
        'cell1 := EmptyCell(10, 15),
        'cell2 := EmptyCell(11, 15),
        'cell3 := EmptyCell(10, 16),
        'cell4 := EmptyCell(9,  15),
        'cell5 := EmptyCell(10, 14)
       ){
        'cell1.call('Cell, 'setEast)('cell2) ~:
        'cell1.call('Cell, 'setSouth)('cell3) ~:
        'cell1.call('Cell, 'setWest)('cell4) ~:
        'cell1.call('Cell, 'setNorth)('cell5) ~:
         debug('cell1.call('Cell, 'getWest)())
       }).toRaw
  }

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
       }).toRaw
  }

   test("Should generate correct tuple access") {
     compile((1, 2, 3).at(1, 3)).code should be (List(LDC(1), LDC(2), LDC(3), CONS, CONS, CAR, RTN))
     compile((1, 2, 3).at(2, 3)).code should be (List(LDC(1), LDC(2), LDC(3), CONS, CONS, CDR, CAR, RTN))
     compile((1, 2, 3).at(3, 3)).code should be (List(LDC(1), LDC(2), LDC(3), CONS, CONS, CDR, CDR, RTN))

     println(compile((1, 2, 3).bind('a, 'b, 'c) { debug('a) }).toRaw)
     println("------------")
  }

  test("Should initialize map") {
    println(compile(letrec(gameObjects.all: _*)(
      'mashUp(list(list(1,0,0), list(1,1,0), list(0,0,1)))
    )).showNumbered)
  }
}