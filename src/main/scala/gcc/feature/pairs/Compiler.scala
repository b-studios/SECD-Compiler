package gcc
package feature.pairs

trait Compiler extends Terms { self: gcc.Compiler =>

  case object CONS extends Instruction
  case object CAR extends Instruction
  case object CDR extends Instruction
  case object ATOM extends Instruction

  def compilePairs: List[Frame] => PartialFunction[Term, Block] = frames => {
    case Cons(head, tail) => compile(head, frames) ++ compile(tail, frames) ++ List(CONS)
    case Car(p)  => compile(p, frames) ++ List(CAR)
    case Cdr(p)  => compile(p, frames) ++ List(CDR)
    case Atom(p) => compile(p, frames) ++ List(ATOM)
  }
}