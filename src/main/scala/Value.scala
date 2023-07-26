package calculator
import calculator.poly.CRatPoly
import calculator.numeric.{NRat, CRat, CRatGen}
import CRatGen.given
import calculator.root.RootFinding

sealed trait Value

case class Root(p: CRatPoly, aval: RBall) extends Value:
    override def toString: String = s"Root($p, $aval)"

    def refine_by (thres: NRat): Root =
        if (aval.rad <= thres) then this
        else
            refine.refine_by(thres)

    def is_zero : Boolean = p.degree == -1 ||
        (p.degree >= 1 && (p.coeffs(0) eqR 0) && aval.c.l2square < aval.rad.square)

    def refine: Root =
        if (aval.rad eqR 0) then this
        else
            def aux(next: List[RBall]): RBall =
                // println(next)
                val n2 = next.map((b) => (b, RootFinding.try_root(p, b))).filter(_._2.forall(_ != 0)).foldLeft[List[(RBall, Option[Int])]](Nil) {
                    case (s, (b, od)) =>  if (od.exists(_ == 1) && s.exists((x) => RootFinding.try_root(p, x._1 merge b).exists(_ == 1))) s else s :+ (b, od)
                }
                // println(n2)
                n2 match
                    case head :: Nil => head._1
                    case _ => aux(RootFinding.refine_balls(n2.map(_._1)))
            Root(p, aux(RootFinding.refine_balls(List(aval))))

end Root

case class Poly(p: CRatPoly) extends Value:
    override def toString: String = s"Poly($p)"
end Poly

