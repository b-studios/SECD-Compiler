package gcc

import scala.collection.mutable

trait Labeling { self: Compiler =>

  private var cnt: Int = 0
  def freshLabel(name: String) = {
    cnt += 1
    UnresolvedLabel(Symbol(s"${name}_$cnt"))
  }

  trait Label { val sym: Symbol }
  case class UnresolvedLabel(sym: Symbol) extends Label {
    override def toString = sym.name
  }
  case class ResolvedLabel(sym: Symbol, address: Int) extends Label {
    override def toString = address.toString
  }
}

trait Global { self: Compiler =>

  val topLevel = mutable.ArrayBuffer.empty[(Label, Block)]
  def addTopLevelBinding(l: Label, b: Block): Unit = {
    topLevel += ((l, b))
  }

  def reset(): Unit = {
    topLevel.clear()
  }

  def topNames = topLevel.map(_._1.sym).toList
  def topBlocks = topLevel.map(_._2).toList
  def validateTopVar(l: Label): Unit = assert(topNames contains l)
}

trait Indexing { self: Labeling =>

    /**
     * @param n Number of binders (frames) to go down
     * @param i Number of variable in the binder
     * @param s Readable name
     */
    case class DeBrujinIdx(n: Int, i: Int, s: Symbol)

    case class Frame(vars: List[Symbol])
    val emptyFrame = Frame(Nil)

    def toIdx(s: Symbol, frames: List[Frame]): DeBrujinIdx = {
      val results = frames.map(varInFrame(s)).zipWithIndex.collect ({
        case (Some(idx), idxFrame) => DeBrujinIdx(idxFrame, idx, s)
      })
      if (results.isEmpty) {
        sys error s"Unbound identifier $s"
      }
      results.head // Finds the left-most (that is, innermost) binding (in case we do support it)
    }

    def varInFrame(s: Symbol)(f: Frame): Option[Int] = {
      val idx = f.vars indexOf s
      if (idx == -1) None else Some(idx)
    }
}

trait Compiler
    extends Labeling
    with Indexing
    with Global
    with feature.booleans.Compiler
    with feature.integers.Compiler
    with feature.sideeffects.Compiler
    with feature.functions.Compiler
    with feature.pairs.Compiler { self: Syntax =>

  type Block = List[Instruction]

  trait Instruction {
    lazy val instrName = this.getClass().getSimpleName().stripSuffix("$")
    override def toString = instrName
  }

  def compile(term: Term): CompiledProgram = {
    reset()
    val main = compile(term, Nil) ++ List(RTN)
    val blocks = main :: topBlocks
    val blockSizes = (blocks.map (_.length) .scanLeft(0)(_ + _)).tail
    val labelSizes = Map(topNames zip blockSizes: _*)
    CompiledProgram(blocks.flatten, labelSizes)
  }

  def compile(term: Term, frames: List[Frame]): Block =
    List(
      compileIntegers,
      compileBooleans,
      compilePairs,
      compileSideeffects,
      compileFunctions) map (f => f(frames)) reduce (_ orElse _) applyOrElse(term, fail)

  private def fail = {
    term: Term => sys error s"Dont know how to generate code for $term"
  }

  case class CompiledProgram(code: Block, labelPositions: Map[Symbol, Int]) {

    def showNumbered = {
      val reveresedPos: Map[Int, Symbol] = labelPositions.map { case (k, v) => (v, k) }

      code.zipWithIndex.map {
        case (el, i) =>
          val label = reveresedPos.get(i).map(s => s"\n${s.name}:\n").getOrElse("")
          val lineStart = s"  ${i}: "
          val opcode = el match {
            case op @ LD(dbIdx)  => op.toString + s"\t\t; var ${dbIdx.s.name}"
            case op @ ST(dbIdx)  => op.toString + s"\t\t; var ${dbIdx.s.name}"
            case other => other.toString
          }
          label + lineStart + opcode
      } mkString "\n"
    }

    def resolveLabelSymbolic(l: Label, labelSizes: Map[Symbol, Int]): Label = l match {
      case UnresolvedLabel(sym) => ResolvedLabel(sym, labelSizes(sym))
    }

    lazy val resolveSymbolic = code map {
      case LDF(rl) =>
        LDF(resolveLabelSymbolic(rl, labelPositions))
      case SEL(thn, els) =>
        SEL(resolveLabelSymbolic(thn, labelPositions), resolveLabelSymbolic(els, labelPositions))
      case op => op
    }

    def toRaw = resolveSymbolic mkString "\n"
  }
}