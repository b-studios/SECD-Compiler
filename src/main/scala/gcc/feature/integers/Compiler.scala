package gcc
package feature.integers

trait Compiler extends Terms { self: gcc.Compiler =>

  case class LDC(n: Int) extends Instruction {
    override def toString = s"LDC $n"
  }

  case object ADD extends Instruction
  case object SUB extends Instruction
  case object MUL extends Instruction
  case object DIV extends Instruction

  case object CEQ extends Instruction
  case object CGT extends Instruction
  case object CGTE extends Instruction

  def compileIntegers: List[Frame] => PartialFunction[Term, Block] = frames => {
    case IntLit(n) => List(LDC(n))
    case Add(l, r) => compile(l, frames) ++ compile(r, frames) ++ List(ADD)
    case Sub(l, r) => compile(l, frames) ++ compile(r, frames) ++ List(SUB)
    case Mul(l, r) => compile(l, frames) ++ compile(r, frames) ++ List(MUL)
    case Div(l, r) => compile(l, frames) ++ compile(r, frames) ++ List(DIV)

    case Eq(l, r)  => compile(l, frames) ++ compile(r, frames) ++ List(CEQ)
    case Gt(l, r)  => compile(l, frames) ++ compile(r, frames) ++ List(CGT)
    case Gte(l, r) => compile(l, frames) ++ compile(r, frames) ++ List(CGTE)
  }
}