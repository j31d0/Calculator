package calculator.property

trait TotalOrder[A] extends Equivalence[A]:
    def < (other: A): Boolean
    def <= (other: A): Boolean = this.eqR(other) || this.<(other)
    def > (other: A): Boolean = !this.<=(other)
    def >= (other: A): Boolean = !this.<(other)
end TotalOrder