package tensorgauge

import chisel3._
import chisel3.util.log2Ceil


class DotProduct(val k: Int, val width: Int, val intermediateWidth: Int = 0)
    extends Module {
  require(k > 0, "k must be positive")
  require(width > 0, "operand width must be positive")

  val fullWidth: Int = 2 * width + log2Ceil(k)
  require(intermediateWidth >= 0 && intermediateWidth <= fullWidth,
    s"intermediateWidth must be 0 (automatic) or 1..$fullWidth")
  val sumWidth: Int = if (intermediateWidth == 0) fullWidth else intermediateWidth

  override def desiredName: String = s"DotProduct_K${k}_W${width}_I${sumWidth}"

  val io = IO(new Bundle {
    val a = Input(Vec(k, UInt(width.W)))
    val b = Input(Vec(k, UInt(width.W)))
    val y = Output(UInt(fullWidth.W))
  })

  val products = Wire(Vec(k, UInt((2 * width).W)))
  val partialSums = Wire(Vec(k, UInt(sumWidth.W)))

  // A Scala loop executes during elaboration: it creates K parallel multipliers.
  for (i <- 0 until k) {
    products(i) := io.a(i) * io.b(i)
  }

  // Explicitly keep low bits or zero-extend; do not rely on implicit truncation.
  private def fitPartialSum(value: UInt): UInt = {
    if (value.getWidth > sumWidth) value(sumWidth - 1, 0)
    else value.pad(sumWidth)
  }

  partialSums(0) := fitPartialSum(products(0))
  // This loop creates K-1 combinational adders, not a multi-cycle software loop
  for (i <- 1 until k) {
    val expandedSum = partialSums(i - 1) +& products(i)
    partialSums(i) := fitPartialSum(expandedSum)
  }
  io.y := partialSums(k - 1).pad(fullWidth)
}
