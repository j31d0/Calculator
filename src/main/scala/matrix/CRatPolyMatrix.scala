package calculator.matrix

import calculator.numeric.{CRat, CRatGen}
import calculator.poly.{CRatPoly, CRatPolyGen}
import calculator.poly.V2Poly

class CRatPolyMatrix(val elems: Array[Array[CRatPoly]]) extends Matrix[CRatPoly, CRatPolyMatrix]:
    assert(elems.length > 0 && elems(0).length > 0)
    val rows: Int = elems.length
    val cols: Int = elems(0).length


given CRatPolyMatrixGen: MatrixGen[CRatPoly, CRatPolyMatrix] with
    def apply(elems: Array[Array[CRatPoly]]): CRatPolyMatrix = new CRatPolyMatrix(elems)

    def bezout(p1: V2Poly, p2: V2Poly): CRatPolyMatrix = 
        val n = if (p1.degree < p2.degree) p2.degree else p1.degree
        val bezout = Array.ofDim[CRatPoly](n, n)
        (0 until n).foreach((i) =>
            (0 until n).foreach((j) => {
                bezout(i)(j) = CRatPolyGen.zero
                (0 to math.min(i, n - 1 - j)).foreach((k) =>
                    bezout(i)(j) = bezout(i)(j) + p1.getCoeff(j + k + 1) * p2.getCoeff(i - k) - p1.getCoeff(i - k) * p2.getCoeff(j + k + 1)
                )
            }
            )
        )
        new CRatPolyMatrix(bezout)