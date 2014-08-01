package gcc
package feature
package derived

import scala.language.implicitConversions

trait Products extends Syntax { self: Labeling =>

  def tuple(firstArg: Term, args: Term*) = (firstArg +: args).reduceRight(cons(_, _))

  // i is 1-based.
  def project(i: Int, n: Int, t: Term): Term =
    if (i < 1)
      sys error s"${i}-th tuple projections are not supported (tuple indexes start from 1)"
    else if (i > n)
      sys error s"Can't project ${i}-th element out of ${n}-ary tuple, valid indexes are from 0 to ${n - 1}"
    else if (n < 2)
      sys error s"${n}-tuples are not supported"
    else {
      if (i == 1)
        t.first
      else if (i == 2 && n == 2)
        t.second
      else
        project(i - 1, n - 1, t.second)
    }

  implicit class ProductOps[T <% Term](t: T) {

    def at(i: Int, n: Int) = project(i, n, t)

    // Allows binding the contents of a tuple - similar to let
    // syntax (1, 2, 3) bind ('a, 'b, 'c) { ... use 'a, 'b and 'c here ... }
    def bind(firstName: Symbol, names: Symbol*)(body: Term) = {
      val tuple = freshLabel("bind").sym
      val size = names.size + 1
      lam(tuple)(let((firstName +: names).zipWithIndex.map {
         case (name, i) => name := tuple.at(i + 1, size)
       }: _*)(body))(t)
    }
  }

  implicit def tuple3ToProduct[S <% Term, T <% Term, U <% Term](p: (S, T, U)): Term =
    tuple(p._1, p._2, p._3)
  implicit def tuple4ToProduct[S <% Term, T <% Term, U <% Term, V <% Term](p: (S, T, U, V)): Term =
    tuple(p._1, p._2, p._3, p._4)
  implicit def tuple5ToProduct[S <% Term, T <% Term, U <% Term, V <% Term, W <% Term](p: (S, T, U, V, W)): Term =
    tuple(p._1, p._2, p._3, p._4, p._5)
  implicit def tuple6ToProductT[S <% Term, T <% Term, U <% Term, V <% Term, W <% Term, X <% Term](p: (S, T, U, V, W, X)): Term =
    tuple(p._1, p._2, p._3, p._4, p._5, p._6)
  implicit def tuple6ToProductT[S <% Term, T <% Term, U <% Term, V <% Term, W <% Term, X <% Term, Y <% Term](p: (S, T, U, V, W, X, Y)): Term =
    tuple(p._1, p._2, p._3, p._4, p._5, p._6, p._7)
  implicit def tuple6ToProductT[S <% Term, T <% Term, U <% Term, V <% Term, W <% Term, X <% Term, Y <% Term, Z <% Term](p: (S, T, U, V, W, X, Y, Z)): Term =
    tuple(p._1, p._2, p._3, p._4, p._5, p._6, p._7, p._8)
}