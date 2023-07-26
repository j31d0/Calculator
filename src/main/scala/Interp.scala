package calculator
import calculator.poly.{CRatPoly, CRatPolyGen, V2Poly, V2PolyGen}
import calculator.numeric.CRatGen
import CRatGen.given
import calculator.root.RootFinding
import calculator.matrix.{CRatPolyMatrix, CRatPolyMatrixGen}
import calculator.numeric.NRat
import calculator.numeric.NPos

object Interp:
    def eval(e: Expr, env: Env): Value = e match
        case Expr.Id(name) => if name == "x" then Poly(CRatPolyGen.from(0, 1)) else env.e(name)
        case Expr.Int(n) => Root(CRatPolyGen.from(-n, 1), RBall(n, 0))
        case Expr.Plus(e1, e2) => (eval(e1, env), eval(e2, env)) match
            case (Poly(p1), Poly(p2)) => Poly(p1 + p2)
            case (Root(p1, a1), Root(p2, a2)) => 
                forward_root(List(Root(p1, a1), Root(p2, a2)), (lp) => poly_add(lp(0), lp(1)), (l) => AInterp.plus(l(0), l(1))).sortWith((a, b) => a.aval.c lt_angle_dist b.aval.c).head
            case _ => ???
        
        case Expr.Minus(e1, e2) => (eval(e1, env), eval(e2, env)) match
            case (Poly(p1), Poly(p2)) => Poly(p1 - p2)
            case (Root(p1, a1), Root(p2, a2)) =>
                val nr2 = Root(p2.substitute(CRatPolyGen.from(0, -1)), RBall(-a2.c, a2.rad))
                forward_root(List(Root(p1, a1), nr2), (lp) => poly_add(lp(0), lp(1)), (l) => AInterp.plus(l(0), l(1))).sortWith((a, b) => a.aval.c lt_angle_dist b.aval.c).head
            case _ => ???
        case Expr.Mult(e1, e2) => (eval(e1, env), eval(e2, env)) match
            case (Poly(p1), Poly(p2)) => Poly(p1 + p2)
            case (Root(p1, a1), Root(p2, a2)) => 
                forward_root(List(Root(p1, a1), Root(p2, a2)), (lp) => poly_mult(lp(0), lp(1)), (l) => AInterp.mult(l(0), l(1))).sortWith((a, b) => a.aval.c lt_angle_dist b.aval.c).head
            case _ => ???
        
        case Expr.Div(e1, e2) => (eval(e1, env), eval(e2, env)) match
            case (Root(p1, a1), Root(p2, a2)) =>
                if (Root(p2, a2).is_zero) then throw new Exception("Divide by zero")
                val nr2 = Root(CRatPolyGen(p2.remove_zeros.coeffs.reverse), AInterp.reciprocal(a2)).refine_by(NRat(1, NPos(1000)))
                forward_root(List(Root(p1, a1), nr2), (lp) => poly_mult(lp(0), lp(1)), (l) => AInterp.mult(l(0), l(1))).sortWith((a, b) => a.aval.c lt_angle_dist b.aval.c).head
            case _ => ???

        case Expr.Neg(e) => eval (e, env) match {
            case (r: Root) => Root(-r.p, RBall(-r.aval.c, r.aval.rad))
            case _ => ???
        }
        case Expr.Sqrt(e) => eval(e, env) match {
            case (r: Root) => backward_root(r, (p) => p.substitute(CRatPolyGen.from(0, 0, 1)), AInterp.square).sortWith((a, b) => a.aval.c lt_angle_dist b.aval.c).head
            case _ => ???
        }
    end eval

    def forward_root(rs: List[Root], ps: List[CRatPoly] => CRatPoly, aforward: List[RBall] => RBall): List[Root] =
        val p = ps(rs.map(_.p)).remove_duproot
        val proots = RootFinding.find_roots(p)
        // println(proots)
        def aux(rs: List[Root], prts: List[Root]): List[Root] =
            val ares = aforward(rs.map(_.aval))
            if (prts.forall ((b) => !ares.intersect(b._2) || b._2.contains(ares))) then
                prts.filter((b) => b._2.contains(ares))
            else aux(rs.map(_.refine), prts)
        aux(rs, proots.map((b) => Root(p, b)))

    def backward_root(rs: Root, ps: CRatPoly => CRatPoly, abackward: RBall => RBall): List[Root] =
        val p = ps(rs.p).remove_duproot
        val proots = RootFinding.find_roots(p)
        // println(proots)
        def aux(rs: Root, prts: List[Root]): List[Root] =
            // println((rs, prts))
            val ares = prts.map((b) => (abackward(b.aval), b))
            if (ares.forall ((b) => !b._1.intersect(rs._2) || RootFinding.try_root(rs.p, b._1).exists(_ == 1))) then
                ares.filter ((b) => b._1.intersect(rs._2)).map(_._2)
            else aux(rs, prts.map(_.refine))
        aux(rs, proots.map((b) => Root(p, b)))

    def poly_add(p1: CRatPoly, p2: CRatPoly): CRatPoly =
        if (p1.degree == -1) return p2
        if (p2.degree == -1) return p1
        val pa1 = p1.lift_v2poly_1
        val pa2 = p2.apply_v2poly(V2PolyGen(Array(CRatPolyGen(Array(0, 1)), CRatPolyGen(Array(-1)))))
        CRatPolyMatrixGen.bezout(pa1, pa2).determinant

    def poly_mult(p1: CRatPoly, p2: CRatPoly): CRatPoly =
        if (p1.degree == -1) return p1
        if (p2.degree == -1) return p2
        val pa1 = p1.lift_v2poly_1
        val pa2 = V2PolyGen(p2.coeffs.zipWithIndex.map((c, i) => if (c eqR 0) CRatPolyGen.zero else CRatPolyGen(Array.tabulate(i + 1)((j) => if (j == i) c else 0))).reverse)
        CRatPolyMatrixGen.bezout(pa1, pa2).determinant
    

end Interp