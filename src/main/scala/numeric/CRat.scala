package calculator.numeric
import calculator.property.Equivalence

class CRat(nr_ : NInt, cr_ : NInt, d_ : NPos) extends Equivalence[CRat] with Field[CRat]:
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

    def compare_distance(other: CRat): Int =
        val a = l2square
        val b = other.l2square
        if a eqR b then 0
        else if a < b then -1
        else 1

    def compare_angle (other: CRat): Int =
        if (cr > 0 || ((cr eqR 0) && nr >= 0)) then
            if (other.cr > 0 || ((other.cr eqR 0) && other.nr >= 0)) then
                val r = NRat(nr, d) * NRat(other.cr, other.d) - NRat(cr, d) * NRat(other.nr, other.d)
                if r eqR 0 then 0
                else if r < 0 then -1
                else 1
            else -1
        else
            if (other.cr > 0 || ((other.cr eqR 0) && other.nr >= 0)) then
                1
            else
                val r = NRat(nr, d) * NRat(other.cr, other.d) - NRat(cr, d) * NRat(other.nr, other.d)
                if r eqR 0 then 0
                else if r < 0 then 1
                else -1

    def lt_angle_dist (other: CRat): Boolean =
        if this eqR other then false
        else
            val a = compare_angle(other)
            if a == 0 then compare_distance(other) < 0
            else a < 0
end CRat


given CRatGen: FieldGen[CRat] with
    def apply(nr: NInt, cr: NInt, d: NPos): CRat = new CRat(nr, cr, d)
    val one = CRat(NInt(1), NInt(0), NPos(1))
    val zero = CRat(NInt(0), NInt(0), NPos(1))


    given fromIntToCRat: Conversion[Int, CRat] with
        def apply(n: Int): CRat = CRat(NInt(n), NInt(0), NPos(1))
    given fromBigIntToCRat: Conversion[BigInt, CRat] with
        def apply(n: BigInt): CRat = CRat(NInt(n), NInt(0), NPos(1))
end CRatGen