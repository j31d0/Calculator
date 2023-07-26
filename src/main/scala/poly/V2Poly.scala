package calculator.poly

import calculator.numeric.CRat

class V2Poly(val coeffs: Array[CRatPoly]) extends Poly[CRatPoly, V2Poly]:
    assert(coeffs.isEmpty || (coeffs.last neqR CRatPolyGen.zero), "last coeff 0")


    override def toString(): String = 
        coeffs.zipWithIndex.map((c, i) => 
            if c eqR CRatPolyGen.zero then ""
            else if i == 0 then c.toString
            else if c eqR CRatPolyGen.one then
                if i == 1 then s"y"
                else s"y^${i}"
            else if i == 1 then s"(${c})y"
            else s"(${c})y^${i}"
        ).filter(_ != "").reverse.mkString(" + ")
    end toString

end V2Poly

given V2PolyGen: PolyGen[CRatPoly, V2Poly] with    
        def apply(coeffs: Array[CRatPoly]): V2Poly = new V2Poly(coeffs)
        def from(coeffs: CRatPoly*): V2Poly =
            new V2Poly(coeffs.toArray)
        end from
end V2PolyGen