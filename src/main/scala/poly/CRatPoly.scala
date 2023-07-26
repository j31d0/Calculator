package calculator.poly
import calculator.numeric.CRat
import calculator.numeric.CRatGen
import calculator.numeric.CRatGen.{given}
import calculator.numeric.NRat
import scala.compiletime.ops.int

class CRatPoly(val coeffs : Array[CRat]) extends Poly[CRat, CRatPoly]:
    assert(coeffs.isEmpty || (coeffs.last neqR 0), "last coeff 0")


    override def toString(): String = 
        coeffs.zipWithIndex.map((c, i) => 
            if c eqR 0 then ""
            else if i == 0 then c.toString
            else if c eqR 1 then
                if i == 1 then s"x"
                else s"x^${i}"
            else if i == 1 then s"${c}x"
            else s"${c}x^${i}"
        ).filter(_ != "").reverse.mkString(" + ")
    end toString

    def remove_duproot: CRatPoly = (this / (this gcd derivative)).normalized

    def remove_zeros: CRatPoly = CRatPolyGen(coeffs.dropWhile(_ eqR 0))

    def derivative: CRatPoly =
        val d = degree
        if d == 0 then CRatPolyGen.from()
        else new CRatPoly(coeffs.zipWithIndex.tail.map((c, i) => c * i))
    end derivative

    def schr: CRatPoly =
        val d = degree
        if d == 0 then CRatPolyGen.from()
        else new CRatPoly(coeffs.map(_ * coeffs(0).conj).reverse.dropWhile(_ eqR 0).reverse) - new CRatPoly(reciprocal.coeffs.map(_.conj * coeffs(d)).reverse.dropWhile(_ eqR 0).reverse)

    def apply_v2poly(arg: V2Poly): V2Poly =
        lift_apply((c) => if (c eqR 0) V2PolyGen.zero else V2PolyGen(Array(CRatPolyGen(Array(c)))))(arg)
    
    def lift_v2poly_0: V2Poly = V2PolyGen(Array(this))
    def lift_v2poly_1: V2Poly = V2PolyGen(coeffs.map((c) => if (c eqR 0) then CRatPolyGen(Array()) else CRatPolyGen(Array(c))))


end CRatPoly

given CRatPolyGen: PolyGen[CRat, CRatPoly] with

    def apply(coeffs: Array[CRat]): CRatPoly = new CRatPoly(coeffs)
    def from(coeffs: CRat*): CRatPoly =
        new CRatPoly(coeffs.toArray)
    end from

    def getSchrChain(p: CRatPoly): List[(NRat, Int)] =
        val tp = p.schr
        // println(tp)
        if (tp.coeffs.forall(_ eqR 0)) Nil
        else (tp(0).toNRat, tp.degree) :: getSchrChain(tp)


end CRatPolyGen