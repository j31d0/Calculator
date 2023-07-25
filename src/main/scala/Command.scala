package calculator
import Expr as EExpr

enum Command:
    case Let(x: String, e: EExpr) extends Command
    case Expr(e: EExpr) extends Command
end Command

object Command:
    def apply(str: String): Command = Parser.parseAll(Parser.cparser, str).getOrElse(Parser.error("Command parse fail"))
end Command