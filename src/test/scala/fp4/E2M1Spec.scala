package tensorgauge.fp4

import org.scalatest.freespec.AnyFreeSpec

class E2M1Spec extends AnyFreeSpec {
  // Explicit table is independent of the model's field-based decoder.
  private val values = Vector(0.0, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 6.0)
  private def value(code: Int): Double = values(code & 7) * (if ((code & 8) == 0) 1 else -1)

  // An independent midpoint quantizer for all quarter-integral arithmetic cases.
  // Doubles represent these small dyadic values exactly.
  private def expected(x: Double): Int = {
    val magnitude = math.abs(x)
    val code = if (magnitude <= 0.25) 0
      else if (magnitude < 0.75) 1
      else if (magnitude <= 1.25) 2
      else if (magnitude < 1.75) 3
      else if (magnitude <= 2.5) 4
      else if (magnitude < 3.5) 5
      else if (magnitude <= 5.0) 6
      else 7
    if (x < 0 && code != 0) code | 8 else code
  }

  "all 16 encodings decode and round-trip, including negative zero" in {
    for (code <- 0 until 16) {
      assert(E2M1.toDouble(code) == value(code))
      assert(E2M1.fromDouble(E2M1.toDouble(code)) == code)
    }
    assert(java.lang.Double.doubleToRawLongBits(E2M1.toDouble(8)) == Long.MinValue)
  }

  "midpoints choose the even code and values just around them choose either neighbor" in {
    for (i <- 0 until 7; sign <- Seq(-1, 1)) {
      val midpoint = (values(i) + values(i + 1)) / 2
      for (x <- Seq(Math.nextDown(midpoint), midpoint, Math.nextUp(midpoint))) {
        val actual = E2M1.encode(BigDecimal(sign * x))
        assert(actual == expected(sign * x), s"rounding $x with sign $sign")
      }
    }
  }

  "conversion saturates, preserves underflow sign, and rejects nonfinite inputs" in {
    assert(E2M1.fromDouble(100) == 7)
    assert(E2M1.fromDouble(-100) == 15)
    assert(E2M1.fromDouble(-0.1) == 8)
    assert(E2M1.fromDouble(0.1) == 0)
    Seq(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach { x =>
      intercept[IllegalArgumentException](E2M1.fromDouble(x))
    }
    Seq(-1, 16).foreach(x => intercept[IllegalArgumentException](E2M1.decode(x)))
  }

  "all 256 addition pairs agree with the independent quantizer" in {
    for (a <- 0 until 16; b <- 0 until 16)
      assert(E2M1.add(a, b) == expected(value(a) + value(b)), s"add($a,$b)")
  }

  "all 256 multiplication pairs agree with the independent quantizer" in {
    for (a <- 0 until 16; b <- 0 until 16)
      assert(E2M1.multiply(a, b) == expected(value(a) * value(b)), s"multiply($a,$b)")
  }

  "all 4096 fused MAC triples agree with the independent quantizer" in {
    for (a <- 0 until 16; b <- 0 until 16; c <- 0 until 16)
      assert(E2M1.fma(a, b, c) == expected(value(a) * value(b) + value(c)), s"fma($a,$b,$c)")
  }

  "fused multiply-accumulate rounds once rather than rounding the product first" in {
    val a = E2M1.fromDouble(1.5)
    val c = E2M1.fromDouble(0.5)
    assert(E2M1.decode(E2M1.fma(a, a, c)) == BigDecimal(3))
    assert(E2M1.decode(E2M1.add(E2M1.multiply(a, a), c)) == BigDecimal(2))
  }

  "all 65536 length-two dot products use the specified lane order" in {
    for (a0 <- 0 until 16; b0 <- 0 until 16; a1 <- 0 until 16; b1 <- 0 until 16) {
      val first = expected(value(a0) * value(b0))
      val wanted = expected(value(a1) * value(b1) + value(first))
      assert(E2M1.dot(Seq(a0, a1), Seq(b0, b1)) == wanted,
        s"dot([$a0,$a1],[$b0,$b1])")
    }
  }

  "dot products expose saturation, cancellation, and accumulation stagnation" in {
    assert(E2M1.dot(Seq(7, 7), Seq(7, 15)) == 15)
    assert(E2M1.exactDot(Seq(7, 7), Seq(7, 15)) == BigDecimal(0))
    assert(E2M1.decode(E2M1.dot(Seq.fill(8)(1), Seq.fill(8)(2))) == BigDecimal(2))
    assert(E2M1.decode(E2M1.dot(Seq(0), Seq(7), initial = 3)) == BigDecimal("1.5"))
    intercept[IllegalArgumentException](E2M1.dot(Seq.empty, Seq.empty))
    intercept[IllegalArgumentException](E2M1.dot(Seq(1), Seq(1, 2)))
  }
}
