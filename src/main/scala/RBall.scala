package calculator
import calculator.numeric.{CRat, CRatGen}
import CRatGen.given
import calculator.numeric.NRat
import calculator.property.Equivalence
import calculator.numeric.NPos
import calculator.numeric.NInt

case class RBall(val c: CRat, val rad: NRat) extends Equivalence[RBall]:
    assert(rad >= 0, "rad must be nonnegative")

    override def toString(): String = s"[$c] ± ${rad}u"

    def eqR (other: RBall): Boolean = (c eqR other.c) && (rad eqR other.rad)

    // not consider tangent case
    def intersect (other: RBall): Boolean =
        val d = c - other.c
        val r = rad + other.rad
        val d2 = NRat((d.cr * d.cr) + (d.nr * d.nr), d.d * d.d)
        d2 < r * r

    // overapproxiamte : if true then Really contains
    def contains (other: RBall): Boolean =
        val d = c - other.c
        d.l1norm + other.rad < rad

    def merge (other: RBall): RBall =
        val d = (c - other.c) / 2
        val r = if (rad < other.rad) other.rad else rad
        val d3 = NRat(NInt(d.cr.n.abs), d.d) + NRat(NInt(d.nr.n.abs), d.d)
        RBall(other.c + d, r + d3)

    def split: List[RBall] =
        val dy: CRat = CRat(0, 1, NPos(1)) * rad
        val dx: CRat = rad
        val nrad: NRat = rad / 2
        List(
            RBall(c + dy, nrad),
            RBall(c - dy, nrad),
            RBall(c + dx, nrad),
            RBall(c - dx, nrad),
            RBall(c, nrad),
            RBall(c + (dy / 2) + (dx / 2), nrad),
            RBall(c + (dy / 2) - (dx / 2), nrad),
            RBall(c - (dy / 2) + (dx / 2), nrad),
            RBall(c - (dy / 2) - (dx / 2), nrad)
        )


end RBall