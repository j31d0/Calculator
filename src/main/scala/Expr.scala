package calculator

import scala.util.parsing.combinator.*


enum Expr:
    case Id(name: String) extends Expr
    case Int(n: BigInt) extends Expr
    case Plus(e1: Expr, e2: Expr) extends Expr
    case Minus(e1: Expr, e2: Expr) extends Expr
    case Mult(e1: Expr, e2: Expr) extends Expr
    case Div(e1: Expr, e2: Expr) extends Expr
    case Neg(e: Expr) extends Expr
    case Sqrt(e: Expr) extends Expr

    override def toString: String = this match {
        case Id(name) => name
        case Int(n) => n.toString
        case Mult(e1, e2) => s"($e1 * $e2)"
        case Div(e1, e2) => s"($e1 / $e2)"
        case Plus(e1, e2) => s"($e1 + $e2)"
        case Minus(e1, e2) => s"($e1 - $e2)"
        case Neg(e) => s"-$e"
        case Sqrt(e) => s"sqrt($e)"
    }

end Expr
