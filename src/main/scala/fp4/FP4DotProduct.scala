package tensorgauge.fp4

import chisel3._

/** Adaptation of the earlier integer DotProduct's combinational lane chain.
  * Each lane uses an exact product plus the preceding FP4 partial sum, then
  * rounds once to E2M1. No registers, handshake, pipeline, or shared scaling.
  */
class FP4DotProduct(val k: Int) extends Module {
  require(k > 0, "k must be positive")
  override def desiredName: String = s"FP4DotProduct_K$k"

  val io = IO(new Bundle {
    val a = Input(Vec(k, UInt(4.W)))
    val b = Input(Vec(k, UInt(4.W)))
    val initial = Input(UInt(4.W))
    val y = Output(UInt(4.W))
    val partialSums = Output(Vec(k, UInt(4.W)))
  })

  val lanes = Seq.fill(k)(Module(new FP4FMA))
  for (i <- 0 until k) {
    lanes(i).io.a := io.a(i)
    lanes(i).io.b := io.b(i)
    lanes(i).io.c := (if (i == 0) io.initial else lanes(i - 1).io.y)
    io.partialSums(i) := lanes(i).io.y
  }
  io.y := lanes.last.io.y
}
