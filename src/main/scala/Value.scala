package calculator
import CRatPoly as PPoly
import calculator.numeric.NRat

sealed trait Value

case class Root(p: PPoly, aval: RBall) extends Value:
    override def toString: String = s"Root($p, $aval)"

    def refine_by (thres: NRat): Root =
        if (aval.rad <= thres) then this
        else
            refine.refine_by(thres)

    def refine: Root =
        if (aval.rad eqR 0) then this
        else
            def aux(next: List[RBall]): RBall =
                // println(next)
                val n2 = next.map((b) => (b, p.try_root(b))).filter(_._2.forall(_ != 0)).foldLeft[List[(RBall, Option[Int])]](Nil) {
                    case (s, (b, od)) =>  if (od.exists(_ == 1) && s.exists((x) => p.try_root(x._1 merge b).exists(_ == 1))) s else s :+ (b, od)
                }
                // println(n2)
                n2 match
                    case head :: Nil => head._1
                    case _ => aux(p.refine_balls(n2.map(_._1)))
            Root(p, aux(p.refine_balls(List(aval))))

end Root

case class Poly(p: PPoly) extends Value:
    override def toString: String = s"Poly($p)"
end Poly

