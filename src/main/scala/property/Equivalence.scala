package calculator.property

trait Equivalence[A]:
    def eqR(other: A): Boolean
    def neqR (other: A): Boolean = !eqR(other)
end Equivalence
