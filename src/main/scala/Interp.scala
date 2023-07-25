package calculator
import CRatPoly as PPoly

object Interp:
    def eval(e: Expr, env: Env): List[Value] = e match
        case Expr.Id(name) => if name == "x" then Poly(PPoly.from(0, 1)):: Nil else env.e(name):: Nil
        case Expr.Int(n) => Root(PPoly.from(-n, 1), RBall(n, 0)):: Nil
        case Expr.Plus(e1, e2) => for {
            x <- eval(e1, env)
            y <- eval(e2, env)
        } yield (x, y) match
            case (Poly(p1), Poly(p2)) => Poly(p1 + p2)
            case _ => ???
        
        case Expr.Minus(e1, e2) => for {
            x <- eval(e1, env)
            y <- eval(e2, env)
         } yield (x, y) match
            case (Poly(p1), Poly(p2)) => Poly(p1 - p2)
            case _ => ???
        case Expr.Mult(e1, e2) => ???
        case Expr.Div(e1, e2) => ???
        case Expr.Neg(e) => ???
        case Expr.Sqrt(e) => eval(e, env) match {
            case (r: Root) :: Nil => backward_root(r, (p) => p.substitute(CRatPoly.from(0, 0, 1)), AInterp.square)
            case _ => ???
        }
    end eval

    def forward_root(rs: List[Root], ps: List[CRatPoly] => CRatPoly, aforward: List[RBall] => RBall): List[Root] =
        val p = ps(rs.map(_.p))
        val proots = p.find_roots
        // println(proots)
        def aux(rs: List[Root], prts: List[Root]): List[Root] =
            val ares = aforward(rs.map(_.aval))
            if (prts.forall ((b) => !ares.intersect(b._2) || b._2.contains(ares))) then
                prts.filter((b) => b._2.contains(ares))
            else aux(rs.map(_.refine), prts)
        aux(rs, proots.map((b) => Root(p, b)))

    def backward_root(rs: Root, ps: CRatPoly => CRatPoly, abackward: RBall => RBall): List[Root] =
        val p = ps(rs.p)
        val proots = p.find_roots
        // println(proots)
        def aux(rs: Root, prts: List[Root]): List[Root] =
            // println((rs, prts))
            val ares = prts.map((b) => (abackward(b.aval), b))
            if (ares.forall ((b) => !b._1.intersect(rs._2) || rs.p.try_root(b._1).exists(_ == 1))) then
                ares.filter ((b) => b._1.intersect(rs._2)).map(_._2)
            else aux(rs, prts.map(_.refine))
        aux(rs, proots.map((b) => Root(p, b)))


end Interp