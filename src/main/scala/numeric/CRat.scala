package calculator.numeric
import calculator.property.Equivalence

class CRat(nr_ : NInt, cr_ : NInt, d_ : NPos) extends Equivalence[CRat]:
    val (nr, cr, d) = if nr_ eqR 0 then
        if cr_ eqR 0 then
            (NInt(0), NInt(0), NPos(1))
        else
            val g = cr_.n.gcd(d_.n)
            (NInt(0), NInt(cr_.n / g), NPos(d_.n / g))
        else
            if cr_ eqR 0 then
                val g = nr_.n.gcd(d_.n)
                (NInt(nr_.n / g), NInt(0), NPos(d_.n / g))
            else
                val g = nr_.n.gcd(d_.n).gcd(cr_.n)
                (NInt(nr_.n / g), NInt(cr_.n / g), NPos(d_.n / g))

    override def toString: String =
        val snu = if cr eqR 0 then s"${nr}" else s"(${nr} + ${cr}i)"
        val sde = if d eqR NPos(1) then "" else s"/${d}"
        s"${snu}${sde}"
    def eqR (other: CRat): Boolean = (nr * other.d eqR other.nr * d) && (cr * other.d eqR other.cr * d)


    def conj: CRat = CRat(nr, -cr, d)

    def + (other: CRat): CRat = CRat(nr * other.d + other.nr * d, cr * other.d + other.cr * d, d * other.d)
    def - (other: CRat): CRat = CRat(nr * other.d - other.nr * d, cr * other.d - other.cr * d, d * other.d)
    def unary_- : CRat = CRat(-nr, -cr, d)
    def * (other: CRat): CRat = CRat((nr * other.nr) - (cr * other.cr), (nr * other.cr) + (cr * other.nr), d * other.d)
    def / (other: CRat): CRat = CRat(((cr * other.cr) + (nr * other.nr)) * other.d, ((cr * other.nr) - (nr * other.cr)) * other.d, (((other.nr * other.nr) + (other.cr * other.cr)) * d).toNPos)


    def l2square : NRat = NRat((nr * nr) + (cr * cr), d * d)
    def l1norm : NRat = NRat(NInt(nr.n.abs + cr.n.abs), d)
    
    def toNRat: NRat =
        assert (cr eqR 0)
        NRat(nr, d)

end CRat


object CRat:
    def apply(nr: NInt, cr: NInt, d: NPos): CRat = new CRat(nr, cr, d)



    given fromIntToCRat: Conversion[Int, CRat] with
        def apply(n: Int): CRat = CRat(NInt(n), NInt(0), NPos(1))
    given fromBigIntToCRat: Conversion[BigInt, CRat] with
        def apply(n: BigInt): CRat = CRat(NInt(n), NInt(0), NPos(1))
end CRat