package tensorgauge.fp4

/** Software specification. Arithmetic uses exact decimal values and enumerates
  * representable candidates; it does not share the RTL's integer thresholds.
  * Encodings: s[3], e[2:1], m[0], exponent bias 1; no NaN or infinity.
  */
object E2M1 {
  val positiveZero = 0x0
  val negativeZero = 0x8

  private def checked(bits: Int): Int = {
    require(bits >= 0 && bits < 16, s"not an FP4 encoding: $bits")
    bits
  }

  def decode(bits: Int): BigDecimal = {
    checked(bits)
    val exponent = (bits >> 1) & 3
    val fraction = bits & 1
    val magnitude = if (exponent == 0) BigDecimal(fraction) / 2
      else (BigDecimal(1) + BigDecimal(fraction) / 2) * BigDecimal(2).pow(exponent - 1)
    if ((bits & 8) != 0) -magnitude else magnitude
  }

  def toDouble(bits: Int): Double =
    if (checked(bits) == negativeZero) -0.0 else decode(bits).toDouble

  /** Round to nearest; even encoding LSB wins a tie. Clamp outside [-6,6].
    * Decimal zero and all arithmetic zero results are canonical +0.
    */
  def encode(value: BigDecimal): Int = {
    val magnitude = value.abs
    val code = (0 until 8).minBy { candidate =>
      ((decode(candidate) - magnitude).abs, candidate & 1)
    }
    if (value.signum < 0 && code != 0) code | 8 else code
  }

  /** Preserve the input sign when conversion rounds to zero. Reject nonfinite
    * host values explicitly: E2M1 has no corresponding encodings.
    */
  def fromDouble(value: Double): Int = {
    require(java.lang.Double.isFinite(value), "E2M1 conversion requires a finite value")
    val code = encode(BigDecimal(value))
    if (code == 0 && java.lang.Double.doubleToRawLongBits(value) < 0) negativeZero else code
  }

  def add(a: Int, b: Int): Int = encode(decode(a) + decode(b))
  def multiply(a: Int, b: Int): Int = encode(decode(a) * decode(b))
  def fma(a: Int, b: Int, c: Int): Int = encode(decode(a) * decode(b) + decode(c))

  /** One fused operation and one FP4 rounding per lane, in input order. */
  def dot(a: Seq[Int], b: Seq[Int], initial: Int = positiveZero): Int = {
    require(a.nonEmpty && a.length == b.length, "dot operands need equal positive length")
    checked(initial)
    a.zip(b).foldLeft(initial) { case (acc, (x, y)) => fma(x, y, acc) }
  }

  /** Exact sum over already encoded inputs, for measuring accumulation error. */
  def exactDot(a: Seq[Int], b: Seq[Int], initial: Int = positiveZero): BigDecimal = {
    require(a.nonEmpty && a.length == b.length)
    a.zip(b).foldLeft(decode(initial)) { case (sum, (x, y)) => sum + decode(x) * decode(y) }
  }
}
