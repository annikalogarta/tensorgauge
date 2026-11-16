package tensorgauge.fp4

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec

/** Simulator checks use encoded matrices and compare every output element. */
class FP4GemmRTLSpec extends AnyFreeSpec with ChiselSim {
  import FP4GemmReference.Matrix

  private def check(dut: FP4Gemm, a: Matrix, b: Matrix, c: Matrix): Unit = {
    for (i <- 0 until dut.m; p <- 0 until dut.k) dut.io.a(i)(p).poke(a(i)(p).U)
    for (p <- 0 until dut.k; j <- 0 until dut.n) dut.io.b(p)(j).poke(b(p)(j).U)
    for (i <- 0 until dut.m; j <- 0 until dut.n) dut.io.c(i)(j).poke(c(i)(j).U)
    val wanted = FP4GemmReference.gemm(a, b, c)
    for (i <- 0 until dut.m; j <- 0 until dut.n) dut.io.d(i)(j).expect(wanted(i)(j).U)
  }

  "scalar GEMM matches all 4096 encoded A/B/C combinations" in {
    simulate(new FP4Gemm(1, 1, 1)) { dut =>
      for (a <- 0 until 16; b <- 0 until 16; c <- 0 until 16)
        check(dut, Vector(Vector(a)), Vector(Vector(b)), Vector(Vector(c)))
    }
  }

  for ((m, n, k) <- Seq((2, 2, 3), (2, 3, 4), (3, 1, 2), (1, 3, 2), (2, 2, 1))) {
    s"GEMM M=$m N=$n K=$k matches directed and seeded random matrices" in {
      simulate(new FP4Gemm(m, n, k)) { dut =>
        val random = new scala.util.Random(20261116L + 100 * m + 10 * n + k)
        def matrix(rows: Int, columns: Int): Matrix =
          Vector.fill(rows, columns)(random.nextInt(16))
        val zerosA = Vector.fill(m, k)(0)
        val zerosB = Vector.fill(k, n)(0)
        val zerosC = Vector.fill(m, n)(0)

        // Distinct C entries and zero products check every accumulator port.
        check(dut, zerosA, zerosB, Vector.tabulate(m, n)((i, j) => (i * n + j) % 16))
        // Saturation then cancellation, and small contributions to a larger C.
        check(dut, Vector.fill(m, k)(7),
          Vector.tabulate(k, n)((p, j) => if ((p + j) % 2 == 0) 7 else 15), zerosC)
        check(dut, Vector.fill(m, k)(1), Vector.fill(k, n)(2), Vector.fill(m, n)(4))
        check(dut, Vector.fill(m, k)(8), Vector.fill(k, n)(15), Vector.fill(m, n)(8))

        // Single active products isolate row/column/inner-index wiring.
        for (row <- 0 until m; inner <- 0 until k; column <- 0 until n) {
          val a = zerosA.updated(row, zerosA(row).updated(inner, 2))
          val b = zerosB.updated(inner, zerosB(inner).updated(column, 3))
          check(dut, a, b, zerosC)
        }
        for (_ <- 0 until 200) check(dut, matrix(m, k), matrix(k, n), matrix(m, n))
      }
    }
  }
}
