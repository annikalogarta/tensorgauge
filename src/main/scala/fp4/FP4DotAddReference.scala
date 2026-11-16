package tensorgauge.fp4

/** Reference for the explicitly defined unscaled E2M1 -> binary32 FDA prototype.
  * fracBits counts fractional positions below the largest term's leading bit.
  * Each term is truncated in magnitude before an exact signed reduction.
  */
object FP4DotAddReference {
  import Binary32.Dyadic
  val maxFracBits = 160
  val maxGroupSize = 64

  private def products(a: Seq[Int], b: Seq[Int]): Seq[Dyadic] = {
    require(a.nonEmpty && a.length == b.length && a.length <= maxGroupSize,
      s"equal operand lengths in 1..$maxGroupSize required")
    a.zip(b).map { case (x, y) =>
      val xHalf = (E2M1.decode(x) * 2).toBigIntExact.get
      val yHalf = (E2M1.decode(y) * 2).toBigIntExact.get
      Dyadic(xHalf * yHalf, -2)
    }
  }

  private def exceptional(c: Long): Option[Long] = {
    Binary32.checked(c)
    if (Binary32.isNaN(c)) Some(Binary32.quietNaN)
    else if (Binary32.isInfinity(c)) Some(c)
    else None
  }

  def exact(a: Seq[Int], b: Seq[Int], c: Long = 0L): Long = {
    val terms = products(a, b)
    exceptional(c).getOrElse(Binary32.round(Binary32.sum(terms :+ Binary32.decodeFinite(c))))
  }

  def apply(a: Seq[Int], b: Seq[Int], c: Long, fracBits: Int): Long = {
    require(fracBits >= 0 && fracBits <= maxFracBits, s"fracBits must be in 0..$maxFracBits")
    val productTerms = products(a, b)
    exceptional(c).getOrElse {
      val terms = (productTerms :+ Binary32.decodeFinite(c)).filter(_.coefficient != 0)
      if (terms.isEmpty) 0L else {
        val commonExponent = terms.map(_.leadingExponent).max
        val retained = terms.map { term =>
          val shift = term.exponent + fracBits - commonExponent
          val magnitude = if (shift >= 0) term.coefficient.abs << shift else term.coefficient.abs >> -shift
          if (term.coefficient < 0) -magnitude else magnitude
        }.sum
        Binary32.round(Dyadic(retained, commonExponent - fracBits))
      }
    }
  }
}
