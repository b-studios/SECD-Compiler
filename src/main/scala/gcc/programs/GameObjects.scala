package gcc
package programs


import Predef.{ any2stringadd => _, _ }
import scala.language.{ implicitConversions, reflectiveCalls }

trait GameObjects { self: GCC =>

 object move {
    val up    = 0
    val right = 1
    val down  = 2
    val left  = 3
  }

  /**
   * The state of the world is encoded as follows:
   *
   * A 4-tuple consisting of
   *
   * 1. The map;
   * 2. the status of Lambda-Man;
   * 3. the status of all the ghosts;
   * 4. the status of fruit at the fruit location.
   *
   * The map is encoded as a list of lists (row-major) representing the 2-d
   * grid. An enumeration represents the contents of each grid cell:
   */
  private lazy val World = class_('World)('world)(
    fun('getMap)()             { 'world.at(1, 4) },
    fun('getLambdaManStatus)() { 'world.at(2, 4) },
    fun('getGhostsStatus)()    { 'world.at(3, 4) },

    /**
     * The status of the fruit is a number which is a countdown to the expiry of
     * the current fruit, if any.
     * - 0: no fruit present;
     * - n > 0: fruit present: the number of game ticks remaining while the
     *          fruit will will be present.
     */
    fun('getFruitStatus)()   { 'world.at(4, 4) },

    fun('update)('newWorld)  { 'world <~ 'newWorld }
  )

  // TODO only create one lambda man and one ghost instance per ghost and try
  //   to update the status, same with fruit

  lazy val item = new {
    val wall        = 0
    val empty       = 1
    val pill        = 2
    val powerpill   = 3
    val fruit       = 4
    val lambdaStart = 5
    val ghostStart  = 6
  }

  /**
   * - 0: Wall (`#`)
   * - 1: Empty (`<space>`)
   * - 2: Pill
   * - 3: Power pill
   * - 4: Fruit location
   * - 5: Lambda-Man starting position
   * - 6: Ghost starting position
   */
  implicit class ItemEnumApi[T <% Term](obj: T) {
    def isWall = { atom(obj) and obj === item.wall }
    def isEmptyField = { atom(obj) and obj === item.empty }
    def isPill = { atom(obj) and obj === item.pill }
    def isPowerPill = { atom(obj) and obj === item.powerpill }
    def isFruit = { atom(obj) and obj === item.fruit }
    def isLambdaStart = { atom(obj) and obj === item.lambdaStart }
    def isGhostStart = { atom(obj) and obj === item.ghostStart }
  }


  /**
   * Class representing twodimensional cells in the game map
   *
   *          north                  ╭────────────────────╮
   *            ┊                    │ timestamp ┆ 177896 │
   *          ┌─┴─┐                  │ distance  ┆     14 │
   *   west ┄┄┤ c ├┄┄ east           │ direction ┆      o┈┾┉┉> ?
   *          └─┬─┘                  │ visited   ┆  false │
   *            ┊                    │ x, y      ┆ (4, 9) │
   *          south                  ╰────────────────────╯
   *
   * The cell is mutable to allow dynamic updating whenever the game state
   * changes.
   */
  private lazy val Cell = class_('Cell)(
    'value,

    // the coordinates of the cell
    'x,
    'y,

    // neighbour cells, might be empty (0)
    'north, //: Cell or empty
    'south, //: Cell or empty
    'east,  //: Cell or empty
    'west,  //: Cell or empty

    // the last updated timestamp
    'timestamp, //: Int

    // the distance and direction to lambda man
    'distance,  //: Int, -1 if not known
    'direction, //: Cell or empty if not known
    'visited    //: Boolean
  )(
    // getters
    getter('value),

    getter('x), getter('y),

    getter('north), getter('south), getter('east), getter('west),

    // TODO specialized setter that automatically set the counterpart
    cellSetter('north), cellSetter('south), cellSetter('east), cellSetter('west),

    getter('timestamp), setter('timestamp),
    getter('distance), setter('distance),
    getter('visited), setter('visited)
  )

  def cellSetter(dir: Symbol) = {
    fun(Symbol("set" + dir.name.capitalize))('cell) {
      if_(not('value.isWall or 'cell.isEmpty)) {
        dir <~ 'cell
      }
    }
  }

  def EmptyCell(value: Term, x: Term, y: Term) = 'new_Cell(value, x, y, 0, 0, 0, 0, -1, -1, 0, false)


  // def setIfNotEmpty(n: Term)

  // initialize the world grid
  // gameMap is assumed to be [[int]]
  private lazy val mashUp = fun('mashUp)('gameMap) (
      'wireRows('gameMap, 0) ~: 'lambdaMan,

      'lambdaMan  := empty, // saves the cell representing the start position
      'lastRow    := empty,

      fun('wireRows)('rows, 'i) (
        'lastRow <~ 'wireCells('lastRow, 'rows.head, empty, empty, 'i, 0) ~:
        'wireRows('rows.tail, 'i + 1)
      ) onlyIf (not('rows.isEmpty)),

      /**
       * Wires the north and east cells of the current cell.
       */
      fun('wireCells)('aboveCells, 'cells, 'lastCell, 'handledCells, 'i, 'j) (
        if_('cells.isEmpty) {
          'handledCells
        } else_ {
          if_(not('aboveCells.isEmpty)) {
            'cell.call('Cell, 'setNorth)('aboveCells.head) ~:
            'aboveCells.head.call('Cell, 'setSouth)('cell)
          } ~:
          if_(not('lastCell.isEmpty)) {
            'cell.call('Cell, 'setWest)('lastCell) ~:
            'lastCell.call('Cell, 'setEast)('cell)
          } ~:
          if_('cells.head.isLambdaStart) {
            'lambdaMan <~ 'cell
          } ~:
          'wireCells('nextAboveCells, 'cells.tail, 'cell, 'cell :: 'handledCells, 'i, 'j + 1)
        },

        'nextAboveCells := if_('aboveCells.isEmpty) { empty } else_ { 'aboveCells.tail },
        'cell := if_ ('cells.isEmpty) { empty } else_ { EmptyCell('cells.head, 'j, 'i) }
      )
  )

  // Exports
  lazy val gameObjects = new {
    val Cell = self.Cell
    val all = Seq(Cell, World, mashUp)
  }
}