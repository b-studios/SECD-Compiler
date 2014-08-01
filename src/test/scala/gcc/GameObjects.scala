package gcc

import org.scalatest.FunSuite
import org.scalatest.Matchers.{ convertSymbolToHavePropertyMatcherGenerator => _, _}

import Predef.{ any2stringadd => _, _ }
import scala.language.{ implicitConversions, reflectiveCalls }

class GameObjectTests extends FunSuite with GCC with programs.Collections  with programs.GameObjects {

  test("game map cells") {
    (compile(
       let(
        gameObjects.Cell,
        collections.reverseList,
        'cell1 := EmptyCell(1, 10, 15),
        'cell2 := EmptyCell(1, 11, 15),
        'cell3 := EmptyCell(1, 10, 16),
        'cell4 := EmptyCell(5, 9,  15),
        'cell5 := EmptyCell(1, 10, 14)
       ){
        ('cell1.east = 'cell2) ~:
        //('cell1.south = 'cell3) ~:
        ('cell1.west = 'cell4) ~:
        //('cell1.north = 'cell5) ~:
         debug('cell1.west.x)
       }).showRaw)
  }


  test("Should initialize map") {
    println(compile(letrec(gameObjects.all :+ collections.reverseList : _*)(
      let(
        'res := 'mashUp(list(
          list(1,1,0),
          list(0,5,1),
          list(0,0,1)))) {

        ('res.distance = 0) ~:
        'optimizePaths(1, list('res)) ~:
        debug('res.x) ~:
        debug('res.y) ~:
        debug('res.value) ~:
        debug('res.north.direction.value) ~:
        debug('res.south) ~:
        debug('res.west) ~:
        debug('res.east.direction.value)
      }

    )).showRaw)
  }

}