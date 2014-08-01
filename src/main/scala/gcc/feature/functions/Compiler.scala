package gcc
package feature.functions

trait Compiler extends Terms { self: gcc.Compiler =>

  case class LDF(label: Label) extends Instruction {
    override def toString = s"LDF $label"
  }
  case class AP(arity: Int) extends Instruction {
    override def toString = s"AP $arity"
  }
  case class LD(idx: DeBrujinIdx) extends Instruction {
    override def toString = s"LD ${idx.n} ${idx.i}"
  }
  case class DUM(n: Int) extends Instruction{
    override def toString = s"DUM $n"
  }
  case class RAP(n: Int) extends Instruction{
    override def toString = s"RAP $n"
  }
  case object RTN extends Instruction

  def toClosure(prefix: String, body: Term, frames: List[Frame]) = {
    // Freshening again would create confusion for human readers between fun_N1 and fun_N2 referring to the same function
    val label = freshLabel(prefix)

    // Add compiled `body` with given name to the list of top-level procedures.
    addTopLevelBinding(label, compile(body, frames) ++ List(RTN))

    // Create the closure here.
    List(LDF(label))
  }

  def compileFunctions: List[Frame] => PartialFunction[Term, Block] = frames => {
    case App(fun, args) => (args :+ fun).flatMap { compile(_, frames) } ++ List(AP(args.size))

    // We have to lift the body to the top.
    // But we don't do lambda-lifting because we still expect to find the variables in the containing frame.
    case Lam(variables, body) => toClosure("fun", body, Frame(variables) :: frames)
    case Var(name) => List(LD(toIdx(name, frames)))

    case LetRec(bindings, body) =>
      val frame = Frame(bindings map (_._1))
      // Allow recursion by binding the names before compiling them.
      val newFrames = frame :: frames
      val labels = bindings flatMap { case (s, t) => compile(t, newFrames) }
      val frameSize = frame.vars.length
      List(DUM(frameSize)) ++ labels ++ toClosure("letrec", body, newFrames) ++ List(RAP(frameSize))
  }
}