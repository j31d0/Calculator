package calculator
import math.Ordered.orderingToOrdered
import calculator.numeric.CRat
import calculator.numeric.NRat
import scala.compiletime.ops.int

class CRatPoly(val coeffs: Array[CRat]):
    assert(coeffs.isEmpty || (coeffs.last neqR 0), "last coeff 0")
    def degree: Int = if coeffs.isEmpty then -1 else coeffs.length - 1

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

    def +(other: CRatPoly): CRatPoly =
        val d1 = degree
        val d2 = other.degree
        val d = d1 max d2
        val newCoeffs: Array[CRat] = Array.fill(d + 1)(0)
        for i <- 0 to d1 do newCoeffs(i) += coeffs(i)
        for i <- 0 to d2 do newCoeffs(i) += other.coeffs(i)
        new CRatPoly(newCoeffs.reverse.dropWhile(_ eqR 0).reverse)
    end +

    def -(other: CRatPoly): CRatPoly =
        val d1 = degree
        val d2 = other.degree
        val d = d1 max d2
        val newCoeffs: Array[CRat] = Array.fill(d + 1)(0)
        for i <- 0 to d1 do newCoeffs(i) += coeffs(i)
        for i <- 0 to d2 do newCoeffs(i) -= other.coeffs(i)
        new CRatPoly(newCoeffs.reverse.dropWhile(_ eqR 0).reverse)
    end -

    def *(other: CRatPoly): CRatPoly =
        val d1 = degree
        val d2 = other.degree
        val d = d1 + d2
        val newCoeffs: Array[CRat] = Array.fill(d + 1)(0)
        for i <- 0 to d1 do
            for j <- 0 to d2 do
                newCoeffs(i + j) += coeffs(i) * other.coeffs(j)
        new CRatPoly(newCoeffs.reverse.dropWhile(_ eqR 0).reverse)
    end *

    def /(other: CRatPoly): CRatPoly =
        val d1 = degree
        val d2 = other.degree
        if d1 < d2 then throw new Exception("degree of divisor must be less than or equal to degree of dividend")
        val newCoeffs: Array[CRat] = Array.fill(d1 - d2 + 1)(0)
        val tempCoeffs = coeffs.clone
        for i <- 0 to d1 - d2 do
            newCoeffs(i) = tempCoeffs(d1 - i) / other.coeffs(d2)
            for j <- 0 to d2 do
                tempCoeffs(d1 - i - j) -= newCoeffs(i) * other.coeffs(d2 - j)
        if tempCoeffs.exists(_ neqR 0) then throw new Exception("remainder is not zero")
        new CRatPoly(newCoeffs.reverse)
    
    def %(other: CRatPoly): CRatPoly =
        val d1 = degree
        val d2 = other.degree
        if d1 < d2 then this
        else
            val newCoeffs: Array[CRat] = Array.fill(d1 - d2 + 1)(0)
            val tempCoeffs = coeffs.clone
            for i <- 0 to d1 - d2 do
                newCoeffs(i) = tempCoeffs(d1 - i) / other.coeffs(d2)
                for j <- 0 to d2 do
                    tempCoeffs(d1 - i - j) -= newCoeffs(i) * other.coeffs(d2 - j)
            new CRatPoly(tempCoeffs)
        

    def substitute(arg: CRatPoly): CRatPoly =
        coeffs.foldRight[CRatPoly](CRatPoly.from()) {
            case (coff, s) => s * arg + (if (coff eqR 0) CRatPoly.from() else CRatPoly.from(coff))
        }
    end substitute


    def derivative: CRatPoly =
        val d = degree
        if d == 0 then CRatPoly.from()
        else new CRatPoly(coeffs.zipWithIndex.tail.map((c, i) => c * i))
    end derivative
    

    def reciprocal: CRatPoly =
        new CRatPoly(coeffs.dropWhile(_ eqR 0).reverse)
    
    def apply(r: CRat): CRat =
        coeffs.foldRight[CRat](0){
            case (coff, s) => s * r + coff
        }

    def schr: CRatPoly =
        val d = degree
        if d == 0 then CRatPoly.from()
        else new CRatPoly(coeffs.map(_ * coeffs(0).conj).reverse.dropWhile(_ eqR 0).reverse) - new CRatPoly(reciprocal.coeffs.map(_.conj * coeffs(d)).reverse.dropWhile(_ eqR 0).reverse)

    def try_1ball: Option[Int] =
        val d = degree
        val schrChain = CRatPoly.getSchrChain(this)
        if (schrChain.isEmpty || !schrChain.forall(_._1 neqR 0) || schrChain.last._2 != 0) None
        else Some(schrChain.zipWithIndex.filter(_._1._1 < 0).foldLeft((0, 1)) {
            case ((s, sign), (_, i)) =>
                val k = if i == 0 then d else schrChain(i-1)._2
                (s + sign * k , sign * -1)
        }._1)
    
    def try_root(ball: RBall): Option[Int] =
        // println((this, ball))
        val r = substitute(CRatPoly.from(ball.c, ball.rad)).try_1ball
        // println(r)
        r

    def find_enclosing_ball: RBall =
        val d = degree
        assert(d > 0)
        def aux(r: NRat): RBall =
            if try_root(RBall(0, r)).exists(_ == d) then RBall(0, r)
            else aux(r * 2)
        aux(1)

    def refine_balls(balls: List[RBall]): List[RBall] =
        balls.flatMap(_.split).distinctBy((b) => (b.c.cr.n, b.c.nr.n, b.c.d.n, b.rad.n.n, b.rad.d.n))

    def find_roots: List[RBall] =
        val ball = find_enclosing_ball
        def aux(next: List[RBall]): List[RBall] =
            // println(next)
            val n2 = next.map((b) => (b, try_root(b))).filter(_._2.forall(_ != 0)).foldLeft[List[(RBall, Option[Int])]](Nil) {
                case (s, (b, od)) =>  if (od.exists(_ == 1) && s.exists((x) => try_root(x._1 merge b).exists(_ == 1))) s else s :+ (b, od)
            }
            val nf = n2.map((b) => (b._1))
            // println(n2)
            if (n2.length <= degree && n2.forall(_._2.exists(_ == 1)) &&
                nf.forall((b1) => nf.forall((b2) =>
                    (b1.c eqR b2.c) || (b1.c - b2.c).l2square > ((b1.rad max b2.rad).square) * 4
                ))) then n2.map(_._1)
            else
                aux(refine_balls(n2.map(_._1)))

        aux(List(ball))
    
    def refine_roots_by(roots: List[RBall], f: RBall => RBall): List[RBall] =
        def aux(next: List[RBall]): List[RBall] =
            // println(next)
            val n2 = next.map((b) => (b, try_root(b))).filter(_._2.forall(_ != 0)).foldLeft[List[(RBall, Option[Int])]](Nil) {
                case (s, (b, od)) =>  if (od.exists(_ == 1) && s.exists((x) => try_root(x._1 merge b).exists(_ == 1))) s else s :+ (b, od)
            }
            val nf = n2.map((b) => f(b._1))
            // println(n2)
            if (n2.length <= degree && n2.forall(_._2.exists(_ == 1)) &&
                nf.forall((b1) => nf.forall((b2) =>
                    (b1.c eqR b2.c) || (b1.c - b2.c).l2square > ((b1.rad max b2.rad).square) * 4
                ))) then n2.map(_._1)
            else
                aux(refine_balls(n2.map(_._1)))

        aux(roots)


    

end CRatPoly

object CRatPoly:
    
    def from(coeffs: CRat*): CRatPoly =
        new CRatPoly(coeffs.toArray)
    end from

    def getSchrChain(p: CRatPoly): List[(NRat, Int)] =
        val tp = p.schr
        // println(tp)
        if (tp.coeffs.forall(_ eqR 0)) Nil
        else (tp(0).toNRat, tp.degree) :: getSchrChain(tp)


end CRatPoly