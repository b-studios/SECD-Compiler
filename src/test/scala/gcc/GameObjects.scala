package gcc

import org.scalatest.FunSuite
import org.scalatest.Matchers.{ convertSymbolToHavePropertyMatcherGenerator => _, _}

import Predef.{ any2stringadd => _, _ }
import scala.language.{ implicitConversions, reflectiveCalls }

class GameObjectTests extends FunSuite with GCC with programs.Collections  with programs.GameObjects {

  test("game map cells") {
    compile(
       let(
        gameObjects.Cell,
        'cell1 := EmptyCell(1, 10, 15),
        'cell2 := EmptyCell(1, 11, 15),
        'cell3 := EmptyCell(1, 10, 16),
        'cell4 := EmptyCell(5, 9,  15),
        'cell5 := EmptyCell(1, 10, 14)
       ){
        'cell1.call('Cell, 'setEast)('cell2) ~:
        //'cell1.call('Cell, 'setSouth)('cell3) ~:
        'cell1.call('Cell, 'setWest)('cell4) ~:
        //'cell1.call('Cell, 'setNorth)('cell5) ~:
         debug('cell1.call('Cell, 'getWest)().call('Cell, 'getX)())
       }).showRaw
  }


  test("Should initialize map") {
    println(compile(letrec(gameObjects.all: _*)(
      debug('mashUp(list(list(1,1,0), list(0,5,1), list(0,0,1))))
    )).showRaw)
  }

}