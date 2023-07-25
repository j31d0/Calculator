package calculator.numeric
import calculator.property.TotalOrder

class NInt(val n: BigInt) extends TotalOrder[NInt]:

    override def toString: String = s"${n}"
    def < (that: NInt): Boolean = n < that.n
    def eqR (that: NInt): Boolean = n == that.n

    def + (other: NInt): NInt = NInt(n + other.n)
    def - (other: NInt): NInt = NInt(n - other.n)
    def unary_- : NInt = NInt(-n)
    def * (other: NInt): NInt = NInt(n * other.n)
    def toNPos: NPos = NPos(n)
end NInt

object NInt:
    def apply(n: BigInt): NInt = new NInt(n)
    given fromIntToNInt: Conversion[Int, NInt] with
        def apply(n: Int): NInt = NInt(n)
    given fromNIntToNRat: Conversion[NInt, NRat] with
        def apply(n: NInt): NRat = NRat(n, NPos(1))
end NInt