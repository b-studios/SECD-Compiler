package gcc
package programs

trait Collections { self: GCC =>

  /**
   * Creates a class with the constructor:
   *
   * @tparam KeyType
   * @tparam ValueType
   *
   * @param store Tree<(KeyType, ValueType)>
   * @param compare KeyType => KeypType => int
   */
  private lazy val TreeMap = class_('TreeMap)('store, 'compare) (

    fun('get)('key) (
      'getHelper('store),

      fun('getHelper)('store) {
        (if_('store.isEmpty) { debug(777) } else_ { noop }) ~:
        let('cmp := 'compare('store.nodeValue.first, 'key)) {
          if_('cmp === 0) {
            'store.nodeValue.second
          }. else_if ('cmp > 0) {
            'getHelper('store.rightTree)
          } else_ {
            'getHelper('store.leftTree)
          }
        }
      }
    ),

    fun('put)('key, 'value) (
      'store <~ 'putHelper('store),

      fun('putHelper)('store) {
        if_('store.isEmpty) {
          leaf(('key, 'value))
        } else_ {
          let('cmp := 'compare('store.nodeValue.first, 'key)) {
            if_('cmp === 0) {
              node('store.leftTree, ('key, 'value), 'store.rightTree)
            }. else_if ('cmp > 0) {
              node('store.leftTree, 'store.nodeValue, 'putHelper('store.rightTree))
            } else_ {
              node('putHelper('store.leftTree), 'store.nodeValue, 'store.rightTree)
            }
          }
        }
      }
    ),

    fun('isDefinedAt)('key) (
      'definedAtHelper('store),

      fun('definedAtHelper)('store) {
        not('store.isEmpty) and
        let('cmp := 'compare('store.nodeValue.first, 'key)) {
          if_('cmp === 0) {
            true
          }. else_if ('cmp > 0) {
            'definedAtHelper('store.rightTree)
          } else_ {
            'definedAtHelper('store.leftTree)
          }
        }
      }
    ),

    fun('print)() { (111111, 'store) },

    fun('isEmpty)() { 'store.isEmpty },

    fun('toList)() (
      'toListHelper('store) ~: 'listResult,

      'listResult := empty,

      fun('toListHelper)('store) {
        if_('store.isEmpty) {
          noop
        } else_ {
          'toListHelper('store.leftTree) ~:
          ('listResult <~ 'store.nodeValue :: 'listResult) ~:
          'toListHelper('store.rightTree)
        }
      }
    )
  )

  private lazy val reverse = fun('reverseList)('l) (
    'reverseHelper('l, lam('x) { 'x }),

    'reverseHelper := lam('l, 'reversed) {
      if_('l.isEmpty) {
        'reversed(empty)
      } else_{
        'reverseHelper('l.tail, lam('tail) {
          'l.head :: 'reversed('tail)
        })
      }
    }
  )

  // Exports
  lazy val collections = new {
    val TreeMap = self.TreeMap
    val reverseList = reverse
    val all = Seq(TreeMap, reverse)
  }
}