package gcc
package feature.sideeffects

trait Compiler extends Terms { self: gcc.Compiler =>

  case object DBUG extends Instruction

  // Store is a prim instruction that pops one value of the stack and stores it into the environment at n i
  case class ST(idx: DeBrujinIdx) extends Instruction {
    override def toString = s"ST ${idx.n} ${idx.i}"
  }

  def compileSideeffects: List[Frame] => PartialFunction[Term, Block] = frames => {
    case Sequence(fst, thn) => compile(fst, frames) ++ compile(thn, frames)
    case Debug(term)        => compile(term, frames) ++ List(DBUG)
    case Noop               => Nil
    case Assign(v, term)    => compile(term, frames) ++ List(ST(toIdx(v, frames)))
  }
}