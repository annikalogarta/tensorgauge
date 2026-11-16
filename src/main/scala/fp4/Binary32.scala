package tensorgauge.fp4

/** Exact software support for a binary32 C/output path. No host floating-point
  * arithmetic is used to round a dyadic value to its binary32 encoding.
  */
object Binary32 {
  final case class Dyadic(coefficient: BigInt, exponent: Int) {
    def leadingExponent: Int = {
      require(coefficient != 0, "zero has no leading exponent")
      exponent + coefficient.abs.bitLength - 1
    }
    def toDouble: Double = java.lang.Math.scalb(coefficient.toDouble, exponent)
  }

  val positiveInfinity = 0x7f800000L
  val quietNaN = 0x7fc00000L

  def checked(bits: Long): Long = {
    require(bits >= 0 && bits <= 0xffffffffL, "binary32 encoding must fit 32 bits")
    bits
  }
  def isNaN(bits: Long): Boolean = {
    checked(bits)
    (bits & 0x7f800000L) == 0x7f800000L && (bits & 0x007fffffL) != 0
  }
  def isInfinity(bits: Long): Boolean = {
    checked(bits)
    (bits & 0x7fffffffL) == positiveInfinity
  }
  def fromFloat(value: Float): Long = java.lang.Float.floatToRawIntBits(value).toLong & 0xffffffffL
  def toFloat(bits: Long): Float = java.lang.Float.intBitsToFloat(checked(bits).toInt)

  def decodeFinite(bits: Long): Dyadic = {
    checked(bits)
    require(!isNaN(bits) && !isInfinity(bits), "finite binary32 value required")
    val exponent = ((bits >>> 23) & 255).toInt
    val fraction = BigInt(bits & 0x007fffffL)
    val magnitude = if (exponent == 0) fraction else fraction + (BigInt(1) << 23)
    Dyadic(if ((bits & 0x80000000L) != 0) -magnitude else magnitude,
      if (exponent == 0) -149 else exponent - 150)
  }

  def sum(terms: Seq[Dyadic]): Dyadic = {
    val nonzero = terms.filter(_.coefficient != 0)
    if (nonzero.isEmpty) Dyadic(BigInt(0), 0) else {
      val exponent = nonzero.map(_.exponent).min
      Dyadic(nonzero.map(t => t.coefficient << (t.exponent - exponent)).sum, exponent)
    }
  }

  private def roundedInteger(magnitude: BigInt, shift: Int): BigInt = {
    if (shift <= 0) magnitude << -shift else {
      val retained = magnitude >> shift
      val remainder = magnitude - (retained << shift)
      val halfway = BigInt(1) << (shift - 1)
      if (remainder > halfway || (remainder == halfway && retained.testBit(0))) retained + 1
      else retained
    }
  }

  /** Round once, nearest/ties-to-even. All rounded zeros are canonical +0;
    * overflow rounds to signed infinity. Subnormals are retained.
    */
  def round(value: Dyadic): Long = {
    if (value.coefficient == 0) return 0L
    val sign = if (value.coefficient < 0) 0x80000000L else 0L
    val magnitude = value.coefficient.abs
    var exponent = value.leadingExponent
    if (exponent < -126) {
      val fraction = roundedInteger(magnitude, -149 - value.exponent)
      if (fraction == 0) 0L else sign | fraction.toLong
    } else {
      var significand = roundedInteger(magnitude, magnitude.bitLength - 24)
      if (significand.bitLength > 24) {
        significand = significand >> 1
        exponent += 1
      }
      if (exponent > 127) sign | positiveInfinity
      else sign | ((exponent + 127).toLong << 23) | (significand.toLong & 0x007fffffL)
    }
  }
}
