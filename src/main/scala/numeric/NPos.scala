package calculator.numeric
import calculator.property.TotalOrder

class NPos(val n: BigInt) extends TotalOrder[NPos]:
    assert(n > 0)
    override def toString: String = s"${n}"

    def < (that: NPos): Boolean = n < that.n
    def eqR (that: NPos): Boolean = n == that.n


    def + (other: NPos): NPos = NPos(n + other.n)
    def * (other: NPos): NPos = NPos(n * other.n)
end NPos


object NPos:
    def apply(n: BigInt): NPos = new NPos(n)
    given fromNPosToNInt: Conversion[NPos, NInt] with
        def apply(n: NPos): NInt = NInt(n.n)
end NPos