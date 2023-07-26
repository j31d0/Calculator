package calculator.matrix

import calculator.numeric.{Field, FieldGen}

trait Matrix[F <: Field[F], M <: Matrix[F, M]](using mg: MatrixGen[F, M], fg: FieldGen[F], evidence: scala.reflect.ClassTag[F]):
    this: M =>
        val rows: Int
        val cols: Int
        val elems: Array[Array[F]]

        
        // using bareiss algorithm, which does not use division
        def determinant: F = {
            if rows != cols then throw new Exception("matrix must be square")
            var tempMatrix = elems
            var prev = fg.one
            for (k <- 0 until rows) {
                val pivot = tempMatrix(k)(k)
                for (i <- k+1 until rows) {
                    for (j <- k+1 until cols) {
                        val tmp = (tempMatrix(i)(j) * pivot) - (tempMatrix(i)(k) * tempMatrix(k)(j))
                        tempMatrix(i)(j) = if (k > 0) tmp / prev else tmp
                    }
                }
                prev = if (k > 0) pivot else prev
            }
            if (rows % 2 == 0) -tempMatrix(rows-1)(cols-1) else tempMatrix(rows-1)(cols-1)
        }

end Matrix


trait MatrixGen[F <: Field[F], M <: Matrix[F, M]]:
    def apply(elems: Array[Array[F]]): M
end MatrixGen