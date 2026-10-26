package tensorgauge

import chisel3.util.log2Ceil

// These are ordinary Scala values used to choose the hardware to generate
final case class DotConfig(k: Int, width: Int, intermediateWidth: Int = 0) {
  require(k > 0 && width > 0)
  val fullWidth: Int = 2 * width + log2Ceil(k)
  require(intermediateWidth >= 0 && intermediateWidth <= fullWidth)
  val sumWidth: Int = if (intermediateWidth == 0) fullWidth else intermediateWidth
  val name: String = s"DotProduct_K${k}_W${width}_I${sumWidth}"
}

final case class DotInput(label: String, a: Seq[BigInt], b: Seq[BigInt])

object Experiments {
  val configs: Seq[DotConfig] = for {
    k <- Seq(4, 8)
    intermediateWidth <- Seq(0, 16, 8)
  } yield DotConfig(k, width = 8, intermediateWidth = intermediateWidth)

  def inputs(c: DotConfig): Seq[DotInput] = {
    val max = (BigInt(1) << c.width) - 1
    Seq(
      DotInput("zero", Seq.fill(c.k)(BigInt(0)), Seq.fill(c.k)(max)),
      DotInput("small", (1 to c.k).map(BigInt(_)), Seq.fill(c.k)(BigInt(2))),
      DotInput("max_operands", Seq.fill(c.k)(max), Seq.fill(c.k)(max)),
      DotInput("one_large_product", max +: Seq.fill(c.k - 1)(BigInt(0)),
        Seq.fill(c.k)(max))
    )
  }
}

/** Software oracle: exact arithmetic followed by ONE modulo, independently of
  * the hardware's chain of truncations. Valid because unsigned modular addition
  * and multiplication preserve the residue at a fixed intermediate width.
  */
object IntegerReference {
  def exact(input: DotInput): BigInt = {
    require(input.a.nonEmpty && input.a.length == input.b.length)
    input.a.zip(input.b).map { case (a, b) => a * b }.sum
  }

  def expected(c: DotConfig, input: DotInput): BigInt = {
    require(input.a.length == c.k && input.b.length == c.k)
    val bound = BigInt(1) << c.width
    require((input.a ++ input.b).forall(x => x >= 0 && x < bound))
    exact(input) % (BigInt(1) << c.sumWidth)
  }
}
