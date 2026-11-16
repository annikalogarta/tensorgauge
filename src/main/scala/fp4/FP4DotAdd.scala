package tensorgauge.fp4

import chisel3._
import chisel3.util._

private[fp4] object FP4DotAddLogic {
  /** Leading exponent of magnitude * 2^lsb; zero is ignored by max reduction. */
  def leading(magnitude: UInt, lsb: SInt): SInt = {
    val index = (magnitude.getWidth - 1).U - PriorityEncoder(Reverse(magnitude))
    Mux(magnitude === 0.U, (-256).S(10.W),
      lsb.pad(10) + Cat(0.U(1.W), index).asSInt.pad(10))
  }

  def align(magnitude: UInt, sign: Bool, lsb: SInt, common: SInt, fracBits: Int): SInt = {
    val width = math.max(24, fracBits + 1)
    val shift = fracBits.S(11.W) + lsb.pad(11) - common.pad(11)
    val left = shift >= 0.S
    val distance = Mux(left, shift, -shift).asUInt
    val amount = distance(math.max(1, log2Ceil(width)) - 1, 0)
    val expanded = magnitude.pad(width)
    val aligned = Mux(distance >= width.U, 0.U(width.W),
      Mux(left, (expanded << amount)(width - 1, 0), expanded >> amount))
    val positive = Cat(0.U(1.W), aligned(fracBits, 0)).asSInt
    Mux(sign, -positive, positive)
  }

  /** Exact nonnegative integer scaled by 2^-shift -> nearest even integer.
    * The caller guarantees the rounded result fits 25 bits.
    */
  private def roundedSignificand(magnitude: UInt, shift: SInt): UInt = {
    val width = math.max(25, magnitude.getWidth)
    val value = magnitude.pad(width)
    val right = shift > 0.S
    val distance = Mux(right, shift, -shift).asUInt
    val amountWidth = math.max(1, log2Ceil(width))
    val amount = distance(amountWidth - 1, 0)
    val quotient = Mux(distance >= width.U, 0.U(width.W), value >> amount)
    val guardIndex = (distance - 1.U)(amountWidth - 1, 0)
    val guard = right && distance <= width.U && (value >> guardIndex)(0)
    val mask = ((1.U((width + 1).W) << guardIndex)(width - 1, 0)) - 1.U
    val sticky = (value & mask) =/= 0.U
    val rounded = quotient +& (guard && (sticky || quotient(0))).asUInt
    val shiftedLeft = Mux(distance >= width.U, 0.U(width.W), (value << amount)(width - 1, 0))
    Mux(right, rounded(24, 0), shiftedLeft(24, 0))
  }

  def pack(sum: SInt, baseExponent: SInt): UInt = {
    val negative = sum < 0.S
    val magnitude = Mux(negative, -sum, sum).asUInt
    val index = (magnitude.getWidth - 1).U - PriorityEncoder(Reverse(magnitude))
    val signedIndex = Cat(0.U(1.W), index).asSInt.pad(12)
    val exponent = baseExponent.pad(12) + signedIndex
    val normal = exponent >= (-126).S
    val shift = Mux(normal, signedIndex - 23.S, (-149).S(12.W) - baseExponent.pad(12))
    val rounded = roundedSignificand(magnitude, shift)
    val carry = rounded(24)
    val finalExponent = exponent + Mux(carry, 1.S(12.W), 0.S(12.W))
    val biased = (finalExponent + 127.S)(7, 0)
    val fraction = Mux(carry, rounded(23, 1), rounded(22, 0))
    val normalBits = Mux(finalExponent > 127.S,
      Cat(negative, 255.U(8.W), 0.U(23.W)), Cat(negative, biased, fraction))
    // rounded bit 23 promotes the largest subnormal to the minimum normal.
    val subnormalBits = Cat(negative, 0.U(7.W), rounded(23, 0))
    Mux(magnitude === 0.U || rounded === 0.U, 0.U(32.W), Mux(normal, normalBits, subnormalBits))
  }
}

/** Unscaled E2M1 K-term fused dot-add with binary32 C/output.
  * fracBits controls internal alignment precision, not the operand format.
  * Terms truncate toward zero, sum exactly with carry headroom, then round once.
  */
class FP4DotAdd(val k: Int, val fracBits: Int) extends Module {
  require(k > 0 && k <= 64, "k must be in 1..64")
  require(fracBits >= 0 && fracBits <= 160, "fracBits must be in 0..160")
  override def desiredName: String = s"FP4DotAdd_K${k}_F$fracBits"

  val io = IO(new Bundle {
    val a = Input(Vec(k, UInt(4.W)))
    val b = Input(Vec(k, UInt(4.W)))
    val c = Input(UInt(32.W))
    val y = Output(UInt(32.W))
  })

  val products = (0 until k).map { i =>
    val product = FP4Logic.halfUnits(io.a(i)) * FP4Logic.halfUnits(io.b(i))
    val sign = product < 0.S
    val magnitude = Mux(sign, -product, product).asUInt(7, 0)
    (magnitude, sign, (-2).S(10.W))
  }
  val cExponent = io.c(30, 23)
  val cSignificand = Cat(cExponent =/= 0.U, io.c(22, 0))
  val cLsb = Mux(cExponent === 0.U, (-149).S(10.W),
    Cat(0.U(1.W), cExponent).asSInt.pad(10) - 150.S(10.W))
  val terms = products :+ ((cSignificand, io.c(31), cLsb))
  val maximum = terms.map { case (magnitude, _, lsb) => FP4DotAddLogic.leading(magnitude, lsb) }
    .reduce((x, y) => Mux(x > y, x, y))
  val common = Mux(maximum === (-256).S, 0.S(10.W), maximum)
  val sumWidth = fracBits + 2 + log2Ceil(k + 1)
  val sum = terms.map { case (magnitude, sign, lsb) =>
    FP4DotAddLogic.align(magnitude, sign, lsb, common, fracBits).pad(sumWidth)
  }.reduce(_ + _)
  val finiteResult = FP4DotAddLogic.pack(sum, common.pad(12) - fracBits.S(12.W))
  io.y := Mux(cExponent === 255.U,
    Mux(io.c(22, 0) =/= 0.U, "h7fc00000".U, io.c), finiteResult)
}
