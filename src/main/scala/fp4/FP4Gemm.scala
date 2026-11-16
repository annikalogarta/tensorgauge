package tensorgauge.fp4

import chisel3._

/** Fixed-size, combinational E2M1 matrix multiply-accumulate.
  * A is M x K, B is K x N, and C/D are M x N.
  * Each output starts at C(i,j), then performs K fused MACs in increasing K
  * order, rounding to FP4 after each MAC. No shared scales or pipeline.
  */
class FP4Gemm(val m: Int, val n: Int, val k: Int) extends Module {
  require(m > 0 && n > 0 && k > 0, "GEMM dimensions must be positive")
  override def desiredName: String = s"FP4Gemm_M${m}_N${n}_K$k"

  val io = IO(new Bundle {
    val a = Input(Vec(m, Vec(k, UInt(4.W))))
    val b = Input(Vec(k, Vec(n, UInt(4.W))))
    val c = Input(Vec(m, Vec(n, UInt(4.W))))
    val d = Output(Vec(m, Vec(n, UInt(4.W))))
  })

  for (row <- 0 until m; column <- 0 until n) {
    val dot = Module(new FP4DotProduct(k))
    dot.io.a := io.a(row)
    for (inner <- 0 until k) dot.io.b(inner) := io.b(inner)(column)
    dot.io.initial := io.c(row)(column)
    io.d(row)(column) := dot.io.y
  }
}
