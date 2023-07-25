package calculator.numeric
import calculator.property.TotalOrder

class NRat(n_ : NInt, d_ : NPos) extends TotalOrder[NRat]:
    val (n, d) = if n_ eqR 0 then (NInt(0), NPos(1)) else
        val g = n_.n.gcd(d_.n)
        (NInt(n_.n / g), NPos(d_.n / g))

    def < (that: NRat): Boolean = 
        val n1 = n * that.d
        val n2 = that.n * d
        n1 < n2
    override def toString: String =
        val sde = if d eqR NPos(1) then "" else s"/${d}"
        s"${n}${sde}"


    def eqR (that: NRat): Boolean = (n * that.d eqR that.n * d)
    def `max` (that: NRat): NRat = if this < that then that else this
    def square : NRat = NRat((n * n), (d * d))


    def + (other: NRat): NRat = NRat(n * other.d + other.n * d, d * other.d)
    def - (other: NRat): NRat = NRat(n * other.d - other.n * d, d * other.d)
    def unary_- : NRat = NRat(-n, d)
    def * (other: NRat): NRat = NRat(n * other.n, d * other.d)
    def / (other: NRat): NRat = NRat(n * other.d, (d * other.n).toNPos)
end NRat


object NRat:
    def apply(n: NInt, d: NPos): NRat = new NRat(n, d)

    given fromIntToNRat: Conversion[Int, NRat] with
        def apply(n: Int): NRat = NRat(NInt(n), NPos(1))
    given fromNRatToCRat: Conversion[NRat, CRat] with
        def apply(p: NRat): CRat = CRat(p.n, 0, p.d)
end NRat