package calculator.numeric
import calculator.property.Equivalence

trait Field[F <: Field[F]] extends Equivalence[F]:
    this: F =>
    def + (other: F): F
    def - (other: F): F
    def unary_- : F
    def * (other: F): F
    def / (other: F): F
end Field

trait FieldGen[F <: Field[F]]:
    val zero: F
    val one: F
end FieldGen

