import scala.annotation.tailrec
import scala.util.matching.Regex

object Day8 extends Base(8) {

  sealed trait Instruction {
    val flippable: Boolean
    def flip: Instruction = ???
  }
  case class Nop(n: Int) extends Instruction {
    override val flippable: Boolean = true
    override def flip: Instruction = Jmp(n)
  }
  case class Acc(inc: Int) extends Instruction {
    override val flippable: Boolean = false
  }
  case class Jmp(steps: Int) extends Instruction {
    override val flippable: Boolean = true
    override def flip: Instruction = Nop(steps)
  }

  val InstructionMatch: Regex = raw"(\w+) ([-+]\d+)".r
  case object Instruction {
    def parse(l: String): Instruction = l match {
      case InstructionMatch("nop", n) => Nop(n.toInt)
      case InstructionMatch("acc", inc) => Acc(inc.toInt)
      case InstructionMatch("jmp", steps) => Jmp(steps.toInt)
    }
  }

  class CPU(instructions: Array[Instruction]) {
    var pos = 0
    var acc = 0

    def terminated(): Boolean =
      pos >= instructions.length

    def step(): Unit = {
      instructions(pos) match {
        case Nop(_) =>
          pos += 1
        case Acc(inc) =>
          acc += inc
          pos += 1
        case Jmp(steps) =>
          pos += steps
      }
    }
  }

  def instructions: Seq[Instruction] = inputLines.map(Instruction.parse)

  case class RunResult(acc: Int, loop: Option[Seq[Int]])

  def getLoop(ins: Array[Instruction]): RunResult = {
    val cpu = new CPU(ins)
    val visits = scala.collection.mutable.Set[Int]()
    var path = Seq[Int]()

    @tailrec
    def runUntilLoop(): Unit = {
      if (!cpu.terminated() && visits.add(cpu.pos)) {
        path = path :+ cpu.pos
        cpu.step()
        runUntilLoop()
      }
    }

    runUntilLoop()

    RunResult(cpu.acc,
      if (cpu.terminated())
        None
      else
        Some(path))
  }

  override def first: Int =
    getLoop(instructions.toArray).acc

  override def second: Int = {
    val ins = instructions
    val flippable = getLoop(ins.toArray)
      .loop.get
      .filter(p => ins(p).flippable)

    flippable.flatMap { p =>
      val newIns = ins.zipWithIndex.map {
        case (_, i) if i == p => ins(p).flip
        case (in, _) => in
      }
      val ret = getLoop(newIns.toArray)
      if (ret.loop.isEmpty) {
        Some(ret.acc)
      } else {
        None
      }
    }.head
  }
}