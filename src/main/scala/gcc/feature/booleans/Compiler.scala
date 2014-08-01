package gcc
package feature.booleans

trait Compiler extends Terms { self: gcc.Compiler =>

  case object JOIN extends Instruction
  case class SEL(thn: Label, els: Label) extends Instruction {
    override def toString = s"""SEL ${List(thn, els) mkString " "}"""
  }

  def compileBooleans: List[Frame] => PartialFunction[Term, Block] = frames => {
    case IfThenElse(cond, thn, els) => {

      val trueLabel = freshLabel("if_t")
      val falseLabel = freshLabel("if_f")

      addTopLevelBinding(trueLabel, compile(thn, frames) ++ List(JOIN))
      addTopLevelBinding(falseLabel, compile(els, frames) ++ List(JOIN))

      compile(cond, frames) ++ List(SEL(trueLabel, falseLabel))
    }
  }

}