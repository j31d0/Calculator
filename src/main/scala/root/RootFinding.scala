package calculator.root

import calculator.poly.{CRatPoly, CRatPolyGen}
import calculator.numeric.CRatGen
import CRatGen.given
import calculator.RBall
import calculator.numeric.NRat

object RootFinding:

    def try_1ball(p: CRatPoly): Option[Int] =
        val d = p.degree
        val schrChain = CRatPolyGen.getSchrChain(p)
        if (schrChain.isEmpty || !schrChain.forall(_._1 neqR 0) || schrChain.last._2 != 0) None
        else Some(schrChain.zipWithIndex.filter(_._1._1 < 0).foldLeft((0, 1)) {
            case ((s, sign), (_, i)) =>
                val k = if i == 0 then d else schrChain(i-1)._2
                (s + sign * k , sign * -1)
        }._1)
    
    def try_root(p: CRatPoly, ball: RBall): Option[Int] =
        // println((this, ball))
        try_1ball(p.substitute(CRatPolyGen.from(ball.c, ball.rad)))

    def find_enclosing_ball(p: CRatPoly): RBall =
        val d = p.degree
        assert(d > 0)
        def aux(r: NRat): RBall =
            if try_root(p, RBall(0, r)).exists(_ == d) then RBall(0, r)
            else aux(r * 2)
        aux(1)

    def refine_balls(balls: List[RBall]): List[RBall] =
        balls.flatMap(_.split).distinctBy((b) => (b.c.cr.n, b.c.nr.n, b.c.d.n, b.rad.n.n, b.rad.d.n))

    def find_roots(p: CRatPoly): List[RBall] =
        val d = p.degree
        val ball = find_enclosing_ball(p)
        def aux(next: List[RBall]): List[RBall] =
            // println(next)
            val n2 = next.map((b) => (b, try_root(p, b))).filter(_._2.forall(_ != 0)).foldLeft[List[(RBall, Option[Int])]](Nil) {
                case (s, (b, od)) =>  if (od.exists(_ == 1) && s.exists((x) => try_root(p, x._1 merge b).exists(_ == 1))) s else s :+ (b, od)
            }
            val nf = n2.map((b) => (b._1))
            // println(n2)
            if (n2.length <= d && n2.forall(_._2.exists(_ == 1)) &&
                nf.forall((b1) => nf.forall((b2) =>
                    (b1.c eqR b2.c) || (b1.c - b2.c).l2square > ((b1.rad max b2.rad).square) * 4
                ))) then n2.map(_._1)
            else
                aux(refine_balls(n2.map(_._1)))

        aux(List(ball))
    
    def refine_roots_by(p: CRatPoly, roots: List[RBall], f: RBall => RBall): List[RBall] =
        val d = p.degree
        def aux(next: List[RBall]): List[RBall] =
            // println(next)
            val n2 = next.map((b) => (b, try_root(p, b))).filter(_._2.forall(_ != 0)).foldLeft[List[(RBall, Option[Int])]](Nil) {
                case (s, (b, od)) =>  if (od.exists(_ == 1) && s.exists((x) => try_root(p, x._1 merge b).exists(_ == 1))) s else s :+ (b, od)
            }
            val nf = n2.map((b) => f(b._1))
            // println(n2)
            if (n2.length <= d && n2.forall(_._2.exists(_ == 1)) &&
                nf.forall((b1) => nf.forall((b2) =>
                    (b1.c eqR b2.c) || (b1.c - b2.c).l2square > ((b1.rad max b2.rad).square) * 4
                ))) then n2.map(_._1)
            else
                aux(refine_balls(n2.map(_._1)))

        aux(roots)


    
end RootFinding
