package tensorgauge.fp4

import org.scalatest.freespec.AnyFreeSpec

class FP4GemmReferenceSpec extends AnyFreeSpec {
  private def enc(rows: Seq[Seq[Double]]): FP4GemmReference.Matrix =
    rows.map(_.map(E2M1.fromDouble).toVector).toVector

  "non-square GEMM uses A rows, B columns, and each distinct C element" in {
    val a = enc(Seq(Seq(1, 0.5, -1), Seq(0, 1, 0.5)))
    val b = enc(Seq(Seq(1, -1), Seq(1, 1), Seq(0.5, 1)))
    val c = enc(Seq(Seq(0.5, 0), Seq(-0.5, 1)))
    assert(FP4GemmReference.gemm(a, b, c) == enc(Seq(Seq(1.5, -1.5), Seq(1, 2))))
  }

  "zero-C matrix multiplication and multiplication by identity preserve exact entries" in {
    val a = enc(Seq(Seq(1, -1, 0.5), Seq(2, 0, -0.5)))
    val identity = enc(Seq(Seq(1, 0, 0), Seq(0, 1, 0), Seq(0, 0, 1)))
    assert(FP4GemmReference.multiply(a, identity) == a)
    val zero = Vector.fill(3, 2)(0)
    assert(FP4GemmReference.multiply(a, zero) == Vector.fill(2, 2)(0))
  }

  "C is seeded before products, with one rounding per fused MAC" in {
    val a = enc(Seq(Seq(0.5, 0.5)))
    val b = enc(Seq(Seq(1), Seq(1)))
    val c = enc(Seq(Seq(2)))
    assert(FP4GemmReference.gemm(a, b, c) == enc(Seq(Seq(2))))
    val roundedProduct = FP4GemmReference.multiply(a, b)(0)(0)
    assert(E2M1.add(roundedProduct, c(0)(0)) == E2M1.fromDouble(3))
    assert(FP4GemmReference.gemm(enc(Seq(Seq(1.5))), enc(Seq(Seq(1.5))),
      enc(Seq(Seq(0.5)))) == enc(Seq(Seq(3))))
  }

  "GEMM retains the baseline saturation, signed-zero, and underflow policies" in {
    assert(FP4GemmReference.multiply(enc(Seq(Seq(6, 6))),
      enc(Seq(Seq(6), Seq(-6)))) == enc(Seq(Seq(-6))))
    assert(FP4GemmReference.gemm(Vector(Vector(8)), Vector(Vector(15)),
      Vector(Vector(8))) == Vector(Vector(0)))
    assert(FP4GemmReference.multiply(enc(Seq(Seq(-0.5))),
      enc(Seq(Seq(0.5)))) == Vector(Vector(0)))
  }

  "seeded rectangular matrices match an independent table-based arithmetic oracle" in {
    val magnitudes = Vector(0.0, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 6.0)
    def value(code: Int): Double = magnitudes(code & 7) * (if (code < 8) 1 else -1)
    def round(x: Double): Int = {
      val code = magnitudes.indices.minBy(i => (math.abs(magnitudes(i) - math.abs(x)), i & 1))
      if (x < 0 && code != 0) code | 8 else code
    }
    val random = new scala.util.Random(20261116L)
    for ((m, n, k) <- Seq((1, 1, 1), (1, 3, 4), (3, 1, 2), (2, 3, 4), (3, 2, 5));
         _ <- 0 until 100) {
      val a = Vector.fill(m, k)(random.nextInt(16))
      val b = Vector.fill(k, n)(random.nextInt(16))
      val c = Vector.fill(m, n)(random.nextInt(16))
      val wanted = Vector.tabulate(m, n) { (i, j) =>
        var acc = c(i)(j)
        for (p <- 0 until k) acc = round(value(a(i)(p)) * value(b(p)(j)) + value(acc))
        acc
      }
      assert(FP4GemmReference.gemm(a, b, c) == wanted, s"M=$m,N=$n,K=$k")
    }
  }

  "empty, ragged, incompatible, and invalid-encoding matrices are rejected" in {
    val one = Vector(Vector(2))
    val badA: Seq[Seq[Seq[Int]]] = Seq(Seq.empty, Seq(Seq.empty),
      Seq(Seq(2), Seq(2, 2)), Seq(Seq(-1)), Seq(Seq(16)))
    badA.foreach(a => intercept[IllegalArgumentException](FP4GemmReference.gemm(a, one, one)))
    badA.foreach(b => intercept[IllegalArgumentException](FP4GemmReference.gemm(one, b, one)))
    badA.foreach(c => intercept[IllegalArgumentException](FP4GemmReference.gemm(one, one, c)))
    intercept[IllegalArgumentException](FP4GemmReference.gemm(Vector(Vector(2, 2)), one, one))
    intercept[IllegalArgumentException](FP4GemmReference.gemm(one, one, Vector(Vector(2, 2))))
  }
}
