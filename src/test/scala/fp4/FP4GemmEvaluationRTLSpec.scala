package tensorgauge.fp4

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec

/** Replays exactly the matrices used to produce the numerical evaluation CSVs. */
class FP4GemmEvaluationRTLSpec extends AnyFreeSpec with ChiselSim {
  for (shape <- FP4GemmEvaluation.shapes) {
    s"evaluation samples match RTL at M=${shape.m} N=${shape.n} K=${shape.k}" in {
      simulate(new FP4Gemm(shape.m, shape.n, shape.k)) { dut =>
        for (sample <- FP4GemmEvaluation.samples.filter(_.shape == shape)) {
          val a = FP4GemmEvaluation.encode(sample.a)
          val b = FP4GemmEvaluation.encode(sample.b)
          val c = FP4GemmEvaluation.encode(sample.c)
          val wanted = FP4GemmReference.gemm(a, b, c)
          for (i <- 0 until shape.m; p <- 0 until shape.k) dut.io.a(i)(p).poke(a(i)(p).U)
          for (p <- 0 until shape.k; j <- 0 until shape.n) dut.io.b(p)(j).poke(b(p)(j).U)
          for (i <- 0 until shape.m; j <- 0 until shape.n) dut.io.c(i)(j).poke(c(i)(j).U)
          for (i <- 0 until shape.m; j <- 0 until shape.n) {
            withClue(s"${sample.pattern}, trial=${sample.trial}, seed=${sample.seed}, D[$i][$j]: ") {
              dut.io.d(i)(j).expect(wanted(i)(j).U)
            }
          }
        }
      }
    }
  }
}
