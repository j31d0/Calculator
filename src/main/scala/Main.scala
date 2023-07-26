package calculator
import org.jline.reader
import org.jline.terminal.TerminalBuilder
import org.jline.utils.AttributedString
import reader.{
  LineReader,
  LineReaderBuilder,
  EndOfFileException,
  EOFError,
  UserInterruptException
}
import scala.collection.mutable

import scala.Console.{MAGENTA => M, CYAN => C, YELLOW => Y, RED => R, RESET}
import calculator.numeric.NRat
import calculator.numeric.NPos

object Main:
  val name = s"calculator"

  def prompt(pname: String) = s"\n$M$pname>$RESET "
  val newLinePrompt = " " * (name.length + 2)

  def main(args: Array[String]): Unit =
    val terminal = TerminalBuilder.builder.dumb(false).build()
    val reader = LineReaderBuilder.builder
      .terminal(terminal)
      // .highlighter(Highlighter)
      // .parser(Parser)
      .variable(LineReader.SECONDARY_PROMPT_PATTERN, "%M")
      .variable(LineReader.HISTORY_FILE, s".${name.toLowerCase}_history")
      .option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
      .build()
    def strs(pname: String): LazyList[String] = (
      try {
        reader.readLine(prompt(pname))
      } catch {
        case _: EndOfFileException | _: UserInterruptException => ":q"
      }
    ) #:: strs(pname)

    println(s"Welcome to the $M$name$RESET REPL.")
    println(s"Type in :q, :quit, or the EOF character to terminate the REPL.")

    var globalEnv: Env = Env(mutable.Map.empty)
    for (str <- strs(name).takeWhile(s => !eof(s)) if str.trim.nonEmpty) {
      val opt = lift {
        val cmd = Command(str)
        // println(s"  ${C}Parsed:$RESET $cmd")
        cmd
      }

      opt.foreach(cmd =>
        cmd match {
          case Command.Let(s, e) =>
            lift (Interp.eval(e, globalEnv) match {
              case r: Root => r.refine_by(NRat(1,NPos(100)))
              case p => p
            }) match
              case None => ()
              case Some(value) =>
                globalEnv.e(s) = value
                println(s" $s = $value")
          case Command.Expr(e) =>
            lift (Interp.eval(e, globalEnv) match {
              case r: Root => r.refine_by(NRat(1,NPos(100)))
              case p => p
            }) match
              case None => 
              case Some(value) =>
                println(s" $value")
        }
      )
    }

  def eof(str: String): Boolean =
    str == ":quit" || str == ":q" || str == "Quit."

  def lift[T](res: => T): Option[T] = try {
    Some(res)
  } catch {
    case ParsingError(msg) =>
      println(s"  Parsing failed. $msg")
      None
    case e: Throwable =>
      e.printStackTrace()
      None
  }
