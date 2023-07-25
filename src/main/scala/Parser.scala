package calculator

import Expr.*
import scala.util.parsing.combinator.RegexParsers

case class ParsingError(msg: String) extends Exception

object Parser extends RegexParsers:
    def error(msg: String): Nothing = throw ParsingError(msg)

    private def wrapR[T](e: => Parser[T]): Parser[T] = "(" ~> e <~ ")"


    lazy val keywords = Set(
      "sqrt"
    )

    private lazy val n: Parser[BigInt] = "-?[0-9]+".r ^^ BigInt.apply
    private lazy val x: Parser[String] =
      "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!keywords(_))

    private lazy val eid: Parser[Id] = x ^^ Id.apply
    private lazy val eint: Parser[Int] = n ^^ Int.apply

    private lazy val e0: Parser[Expr] =
        eid |
        eint |
        wrapR(eparser) |
        "sqrt" ~> wrapR(eparser) ^^ Sqrt.apply

    private lazy val e1: Parser[Expr] =
        e0 |
        "-" ~> (eid | wrapR(eparser)) ^^ Neg.apply
    
    enum E1Op:
        case Mult, Div
    
    private lazy val e1op: Parser[E1Op] =
        "*" ^^^ E1Op.Mult |
        "/" ^^^ E1Op.Div
    
    private lazy val e2: Parser[Expr] =
        e1 ~ rep ( e1op ~ e1 ) ^^ {
          case e0 ~ ope0s => ope0s.foldLeft(e0) {
            case (e1, ope0 ~ e0) => ope0 match {
              case E1Op.Mult => Mult(e1, e0)
              case E1Op.Div => Div(e1, e0)
            }
          }
      }

    enum E2Op:
        case Plus, Minus
    
    private lazy val e2op: Parser[E2Op] =
        "+" ^^^ E2Op.Plus |
        "-" ^^^ E2Op.Minus
    
    private lazy val e3: Parser[Expr] =
        e2 ~ rep ( e2op ~ e2 ) ^^ {
          case e1 ~ ope1s => ope1s.foldLeft(e1) {
            case (e2, ope1 ~ e1) => ope1 match {
              case E2Op.Plus => Plus(e2, e1)
              case E2Op.Minus => Minus(e2, e1)
            }
          }
      }

    lazy val eparser: Parser[Expr] = e3


    private lazy val clet: Parser[Command.Let] = ("Let" ~> x <~ ":=") ~ (eparser <~ ";") ^^ { case (s ~ e) => Command.Let(s, e) }
    private lazy val cexpr: Parser[Command.Expr] = (eparser <~ ";") ^^ { case e => Command.Expr(e) }

    lazy val cparser: Parser[Command] = clet | cexpr

end Parser