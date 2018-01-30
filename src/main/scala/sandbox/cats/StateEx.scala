package sandbox.cats

import cats.data.State

import scala.util.Try

object StateEx {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = {
    def pushOperand(value: Int): CalcState[Int] =
      State(oldStack => (value :: oldStack, value))

    def performOperation: CalcState[Int] = {
      val op: (Int, Int) => Int = sym match {
        case "+" => _ + _
        case "-" => _ - _
        case "*" => _ * _
        case "/" => _ / _
      }

      State {
        case p2 :: p1 :: rest =>
          val opRes = op(p1, p2)
          (opRes :: rest, opRes)
        case _ => sys.error("Stack does not have two operands in it.")
      }
    }

    Try(sym.toInt).map(pushOperand).recover { case ex => performOperation }.get
  }

  // same as above, just use primitive constructor sequencing
  def evalOnePc(sym: String): CalcState[Int] =
    for {
      _ <- State.modify[List[Int]](transformStack(_, sym))
      r <- State.inspect[List[Int], Int](_.head)
    } yield r

  def transformStack(oldStack: List[Int], sym: String): List[Int] = {
    Try(sym.toInt)
      .map(_ :: oldStack)
      .recover {
        case ex =>
          val (p2 :: p1 :: restStack) = oldStack
          val opRes = sym match {
            case "+" => p1 + p2
            case "-" => p1 - p2
            case "*" => p1 * p2
            case "/" => p1 / p2
          }
          opRes :: restStack
      }
      .get
  }

  import cats.syntax.applicative._

  def evalAll(syms: Seq[String]): CalcState[Int] =
    syms.foldLeft(0.pure[CalcState])((accS, sym) => accS.flatMap(_ => evalOne(sym)))

  def main(args: Array[String]): Unit = {
    val oneRes = evalOne("23").run(Nil).value
    println(s"oneRes: $oneRes")

    val oneResPc = evalOnePc("-23").run(Nil).value
    println(s"oneResPc: $oneResPc")

    val progText = "2 1 + 3 2 + *"
    val prog = evalAll(progText.split(" ").toSeq)

    val progVal = prog.run(Nil).value
    println(s"'$progText' evaluates to $progVal")
  }
}
