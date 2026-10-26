package tensorgauge.fp4

import chisel3._
import chisel3.util._

/** Hardware implementation of E2M1 arithmetic using exact integer intermediates.
  * Decoding gives 2*x, so multiplication gives 4*x*y without precision loss.
  * This is a format-specific baseline, not a configurable floating-point unit.
  */
private[fp4] object FP4Logic {
  def halfUnits(bits: UInt): SInt = {
    val magnitude = VecInit(Seq(0, 1, 2, 3, 4, 6, 8, 12).map(_.S(5.W)))(bits(2, 0))
    Mux(bits(3), -magnitude, magnitude)
  }

  /** Input is an exact signed count of quarter units. Adjacent midpoints are
    * integral in these units; even magnitude codes include the tie boundary.
    */
  def roundQuarterUnits(value: SInt): UInt = {
    val negative = value < 0.S
    val magnitude = Mux(negative, -value, value).asUInt
    val code = WireDefault(7.U(3.W))
    val midpoints = Seq(1, 3, 5, 7, 10, 14, 20)
    for (i <- 6 to 0 by -1) {
      val select = if ((i & 1) == 0) magnitude <= midpoints(i).U
        else magnitude < midpoints(i).U
      when(select) { code := i.U }
    }
    // Baseline arithmetic canonicalizes every rounded zero to +0.
    Cat(negative && code =/= 0.U, code)
  }

  def add(a: UInt, b: UInt): UInt =
    roundQuarterUnits((halfUnits(a) +& halfUnits(b)) << 1)

  def multiply(a: UInt, b: UInt): UInt =
    roundQuarterUnits(halfUnits(a) * halfUnits(b))

  def fma(a: UInt, b: UInt, c: UInt): UInt = {
    val exactProduct = halfUnits(a) * halfUnits(b)
    val exactAccumulator = halfUnits(c) << 1
    roundQuarterUnits(exactProduct +& exactAccumulator)
  }
}

class FP4Decode extends Module {
  val io = IO(new Bundle {
    val bits = Input(UInt(4.W))
    val sign = Output(Bool())
    val exponent = Output(UInt(2.W))
    val fraction = Output(Bool())
    val isZero = Output(Bool())
    val isSubnormal = Output(Bool())
    val halfUnits = Output(SInt(5.W))
  })
  io.sign := io.bits(3)
  io.exponent := io.bits(2, 1)
  io.fraction := io.bits(0)
  io.isZero := io.bits(2, 0) === 0.U
  io.isSubnormal := io.bits(2, 1) === 0.U && io.bits(0)
  io.halfUnits := FP4Logic.halfUnits(io.bits)
}

class FP4Add extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(4.W))
    val b = Input(UInt(4.W))
    val y = Output(UInt(4.W))
  })
  io.y := FP4Logic.add(io.a, io.b)
}

class FP4Multiply extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(4.W))
    val b = Input(UInt(4.W))
    val y = Output(UInt(4.W))
  })
  io.y := FP4Logic.multiply(io.a, io.b)
}

class FP4FMA extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(4.W))
    val b = Input(UInt(4.W))
    val c = Input(UInt(4.W))
    val y = Output(UInt(4.W))
  })
  io.y := FP4Logic.fma(io.a, io.b, io.c)
}
