package calculator.poly

import calculator.numeric.{Field, FieldGen}
import scala.reflect.ClassTag

trait Poly[F <: Field[F], T <: Poly[F, T]](using pg: PolyGen[F, T], fg: FieldGen[F], evidence: scala.reflect.ClassTag[F]) extends Field[T]:
    this: T =>
    val coeffs: Array[F]
    def degree: Int = if coeffs.isEmpty then -1 else coeffs.length - 1


    def eqR(other: T): Boolean = 
        val d1 = degree
        val d2 = other.degree
        if d1 != d2 then false
        else coeffs.zip(other.coeffs).forall((c1, c2) => c1 eqR c2)

    def normalized(using pg: PolyGen[F, T], evidence: scala.reflect.ClassTag[F]): T =
        val d = degree
        if d == -1 || d == 0 then this
        else
            val g = coeffs(d)
            pg(coeffs.map(_ / g))

    def +(other: T): T =
        val d1 = degree
        val d2 = other.degree
        val d = d1 max d2
        val newCoeffs: Array[F] = Array.fill(d + 1)(fg.zero)
        for i <- 0 to d1 do newCoeffs(i) += coeffs(i)
        for i <- 0 to d2 do newCoeffs(i) += other.coeffs(i)
        pg(newCoeffs.reverse.dropWhile(_ eqR fg.zero).reverse)
    end +

    def -(other: T): T =
        val d1 = degree
        val d2 = other.degree
        val d = d1 max d2
        val newCoeffs: Array[F] = Array.fill(d + 1)(fg.zero)
        for i <- 0 to d1 do newCoeffs(i) += coeffs(i)
        for i <- 0 to d2 do newCoeffs(i) -= other.coeffs(i)
        pg(newCoeffs.reverse.dropWhile(_ eqR fg.zero).reverse)
    end -

    def unary_- : T =
        val d = degree
        val newCoeffs: Array[F] = Array.fill(d + 1)(fg.zero)
        for i <- 0 to d do newCoeffs(i) -= coeffs(i)
        pg(newCoeffs.reverse.dropWhile(_ eqR fg.zero).reverse)

    def *(other: T): T =
        val d1 = degree
        val d2 = other.degree
        val d = d1 + d2
        val newCoeffs: Array[F] = Array.fill(d + 1)(fg.zero)
        for i <- 0 to d1 do
            for j <- 0 to d2 do
                newCoeffs(i + j) += coeffs(i) * other.coeffs(j)
        pg(newCoeffs.reverse.dropWhile(_ eqR fg.zero).reverse)
    end *

    def /(other: T): T =
        val d1 = degree
        val d2 = other.degree
        if d1 < d2 then throw new Exception("degree of divisor must be less than or equal to degree of dividend")
        val newCoeffs: Array[F] = Array.fill(d1 - d2 + 1)(fg.zero)
        val tempCoeffs = coeffs.clone
        for i <- 0 to d1 - d2 do
            newCoeffs(i) = tempCoeffs(d1 - i) / other.coeffs(d2)
            for j <- 0 to d2 do
                tempCoeffs(d1 - i - j) -= newCoeffs(i) * other.coeffs(d2 - j)
        if tempCoeffs.exists(_ neqR fg.zero) then throw new Exception("remainder is not zero")
        pg(newCoeffs.reverse)
    
    def %(other: T): T =
        val d1 = degree
        val d2 = other.degree
        if d1 < d2 then this
        else
            val newCoeffs: Array[F] = Array.fill(d1 - d2 + 1)(fg.zero)
            val tempCoeffs = coeffs.clone
            for i <- 0 to d1 - d2 do
                newCoeffs(i) = tempCoeffs(d1 - i) / other.coeffs(d2)
                for j <- 0 to d2 do
                    tempCoeffs(d1 - i - j) -= newCoeffs(i) * other.coeffs(d2 - j)
            pg(tempCoeffs.reverse.dropWhile(_ eqR fg.zero).reverse)
    
    def getCoeff(i: Int): F =
        if i < 0 then throw new Exception("index must be non-negative")
        else if i > degree then fg.zero
        else coeffs(i)

    def gcd (other: T): T =
        val d1 = degree
        val d2 = other.degree
        if d1 < d2 then other gcd this
        else if d2 == -1 then this
        else
            val r = this % other
            if r.degree == -1 then other
            else other gcd r

    def reciprocal: T =
        pg(coeffs.dropWhile(_ eqR fg.zero).reverse)
    
    def apply(r: F): F =
        coeffs.foldRight[F](fg.zero){
            case (coff, s) => s * r + coff
        }

    def lift_apply[F2 <: Field[F2]](f: F => F2)(arg: F2)(implicit fg2: FieldGen[F2]): F2 =
        coeffs.foldRight[F2](fg2.zero){
            case (coff, s) => s * arg + f(coff)
        }
    
    def substitute(arg: T): T =
        coeffs.foldRight[T](pg(Array())) {
            case (coff, s) => s * arg + (if (coff eqR fg.zero) pg(Array())
            else pg(Array(coff)))
        }

    end substitute

end Poly

trait PolyGen[F <: Field[F], T <: Poly[F, T]](implicit fg: FieldGen[F], evidence: scala.reflect.ClassTag[F]) extends FieldGen[T]:
    val one = apply(Array(fg.one))
    val zero = apply(Array())
    def apply(coeffs: Array[F]): T

end PolyGen