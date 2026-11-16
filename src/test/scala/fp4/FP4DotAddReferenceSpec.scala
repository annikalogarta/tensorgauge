package tensorgauge.fp4

import org.scalatest.freespec.AnyFreeSpec

class FP4DotAddReferenceSpec extends AnyFreeSpec {
  import Binary32.Dyadic
  private def canonical(value: Float): Long =
    if (value.isNaN) Binary32.quietNaN else if (value == 0.0f) 0L else Binary32.fromFloat(value)

  "binary32 finite encodings round-trip, including subnormals and exponent extremes" in {
    val random = new scala.util.Random(20261117L)
    val directed = Seq(0L, 0x80000000L, 1L, 0x80000001L, 0x007fffffL, 0x00800000L,
      0x3f800000L, 0x7f7fffffL, 0xff7fffffL)
    val cases = directed ++ Seq.fill(10000)(random.nextInt().toLong & 0xffffffffL)
    for (bits <- cases if !Binary32.isNaN(bits) && !Binary32.isInfinity(bits)) {
      val wanted = if ((bits & 0x7fffffffL) == 0) 0L else bits
      assert(Binary32.round(Binary32.decodeFinite(bits)) == wanted, f"bits=0x$bits%08x")
    }
  }

  "binary32 rounds ties to even across normal, subnormal, carry, and overflow boundaries" in {
    assert(Binary32.round(Dyadic((BigInt(1) << 24) + 1, -24)) == 0x3f800000L)
    assert(Binary32.round(Dyadic((BigInt(1) << 24) + 3, -24)) == 0x3f800002L)
    assert(Binary32.round(Dyadic((BigInt(1) << 25) - 1, -24)) == 0x40000000L)
    assert(Binary32.round(Dyadic(BigInt(1), -150)) == 0L)
    assert(Binary32.round(Dyadic(BigInt(-1), -150)) == 0L)
    assert(Binary32.round(Dyadic(BigInt(3), -150)) == 2L)
    assert(Binary32.round(Dyadic((BigInt(1) << 24) - 1, -150)) == 0x00800000L)
    assert(Binary32.round(Dyadic((BigInt(1) << 25) - 2, 103)) == 0x7f7fffffL)
    assert(Binary32.round(Dyadic((BigInt(1) << 25) - 1, 103)) == Binary32.positiveInfinity)
    assert(Binary32.round(Dyadic(-((BigInt(1) << 25) - 1), 103)) == 0xff800000L)
  }

  "full-retention scalar dot-add agrees with the JVM's independently rounded float FMA" in {
    val cValues = Seq(0L, 0x80000000L, 1L, 0x80000001L, 0x007fffffL, 0x00800000L,
      0x3f000000L, 0xbf000000L, 0x42100000L, 0xc2100000L, 0x4b800000L,
      0x4b800001L, 0x7f7fffffL, 0xff7fffffL, 0x7f800000L, 0xff800000L, 0x7fa12345L)
    for (a <- 0 until 16; b <- 0 until 16; c <- cValues) {
      val wanted = canonical(java.lang.Math.fma(E2M1.toDouble(a).toFloat,
        E2M1.toDouble(b).toFloat, Binary32.toFloat(c)))
      assert(FP4DotAddReference(Seq(a), Seq(b), c, 160) == wanted, s"a=$a,b=$b,c=$c")
    }
    val random = new scala.util.Random(20261118L)
    for (_ <- 0 until 2000) {
      val a = random.nextInt(16)
      val b = random.nextInt(16)
      val c = random.nextInt().toLong & 0xffffffffL
      assert(FP4DotAddReference.exact(Seq(a), Seq(b), c) == canonical(java.lang.Math.fma(
        E2M1.toDouble(a).toFloat, E2M1.toDouble(b).toFloat, Binary32.toFloat(c))))
    }
  }

  "narrow alignment truncates signed magnitudes toward zero" in {
    assert(FP4DotAddReference(Seq(3), Seq(2), 0L, 0) == Binary32.fromFloat(1.0f))
    assert(FP4DotAddReference(Seq(11), Seq(2), 0L, 0) == Binary32.fromFloat(-1.0f))
    assert(FP4DotAddReference(Seq(3), Seq(2), 0L, 1) == Binary32.fromFloat(1.5f))
  }

  "additional retained bits recover a small product after cancellation" in {
    val a = Seq(7, 1, 15)
    val b = Seq(7, 1, 7)
    assert(FP4DotAddReference(a, b, 0L, 0) == 0L)
    assert(FP4DotAddReference(a, b, 0L, 7) == Binary32.fromFloat(0.25f))
    assert(E2M1.decode(E2M1.dot(a, b)) == BigDecimal(-6))
    assert(FP4DotAddReference(Seq(7, 1), Seq(7, 1), Binary32.fromFloat(-36.0f), 7) ==
      Binary32.fromFloat(0.25f))
    assert(FP4DotAddReference(Seq(7, 15), Seq(7, 7), 1L, 24) == 0L)
    assert(FP4DotAddReference(Seq(7, 15), Seq(7, 7), 1L, 160) == 1L)
  }

  "FP32 C is included before one final rounding" in {
    assert(FP4DotAddReference(Seq(2), Seq(2), 0x4b800000L, 24) == 0x4b800000L)
    assert(FP4DotAddReference(Seq(2), Seq(2), 0x4b800001L, 24) == 0x4b800002L)
    assert(FP4DotAddReference(Seq(2), Seq(2), 0x4b800001L, 23) == 0x4b800001L)
  }

  "160 fractional positions preserve all finite input terms before final rounding" in {
    val random = new scala.util.Random(20261119L)
    for (k <- Seq(1, 4, 8, 64); _ <- 0 until 100) {
      val a = Seq.fill(k)(random.nextInt(16))
      val b = Seq.fill(k)(random.nextInt(16))
      val c = random.nextInt().toLong & 0xffffffffL
      assert(FP4DotAddReference(a, b, c, 160) == FP4DotAddReference.exact(a, b, c))
    }
    assert(FP4DotAddReference(Seq.fill(64)(7), Seq.fill(64)(7), 0L, 160) ==
      Binary32.fromFloat(2304.0f))
  }

  "exceptional C and arithmetic zeros follow the declared contract" in {
    assert(FP4DotAddReference(Seq(7), Seq(7), 0x7f800000L, 0) == 0x7f800000L)
    assert(FP4DotAddReference(Seq(7), Seq(7), 0xff800000L, 0) == 0xff800000L)
    assert(FP4DotAddReference(Seq(7), Seq(7), 0xffa12345L, 0) == Binary32.quietNaN)
    assert(FP4DotAddReference(Seq(8), Seq(7), 0x80000000L, 0) == 0L)
  }

  "invalid dimensions, widths, and encodings are rejected" in {
    intercept[IllegalArgumentException](FP4DotAddReference(Seq.empty, Seq.empty, 0L, 0))
    intercept[IllegalArgumentException](FP4DotAddReference(Seq(1), Seq(1, 2), 0L, 0))
    intercept[IllegalArgumentException](FP4DotAddReference(Seq.fill(65)(1), Seq.fill(65)(1), 0L, 0))
    for (p <- Seq(-1, 161))
      intercept[IllegalArgumentException](FP4DotAddReference(Seq(1), Seq(1), 0L, p))
    intercept[IllegalArgumentException](FP4DotAddReference(Seq(16), Seq(1), 0L, 0))
    intercept[IllegalArgumentException](FP4DotAddReference(Seq(1), Seq(1), 0x100000000L, 0))
    intercept[IllegalArgumentException](Binary32.decodeFinite(Binary32.quietNaN))
  }
}
