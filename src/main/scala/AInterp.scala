package calculator
import math.Ordering.Implicits.infixOrderingOps

object AInterp:

    
    def plus(a1: RBall, a2: RBall): RBall = new RBall(
        a1.c + a2.c,
        (a1.rad + a2.rad)
    )
    def minus(a1: RBall, a2: RBall): RBall = new RBall(
        a1.c - a2.c,
        (a1.rad + a2.rad)
    )

    def mult(a1: RBall, a2: RBall): RBall = new RBall(
        a1.c * a2.c,
        (a1.rad * a2.rad) + (a1.c.l1norm * a2.rad) + (a2.c.l1norm * a1.rad)
    )

    def square(a: RBall): RBall = mult(a, a)


end AInterp
    