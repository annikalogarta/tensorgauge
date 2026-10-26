package tensorgauge.fp4

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec

/** Requires a working native simulator (Verilator). Software-only tests and
  * elaboration are separate suites so they can run without that toolchain.
  */
class FP4RTLSpec extends AnyFreeSpec with ChiselSim {
  "RTL decoder matches every E2M1 encoding" in {
    simulate(new FP4Decode) { dut =>
      for (bits <- 0 until 16) {
        dut.io.bits.poke(bits.U)
        dut.io.halfUnits.expect((E2M1.decode(bits) * 2).toInt.S)
        dut.io.sign.expect(((bits & 8) != 0).B)
        dut.io.exponent.expect(((bits >> 1) & 3).U)
        dut.io.fraction.expect(((bits & 1) != 0).B)
        dut.io.isZero.expect(((bits & 7) == 0).B)
        dut.io.isSubnormal.expect(((bits & 7) == 1).B)
      }
    }
  }

  "RTL addition matches all 256 reference pairs" in {
    simulate(new FP4Add) { dut =>
      for (a <- 0 until 16; b <- 0 until 16) {
        dut.io.a.poke(a.U)
        dut.io.b.poke(b.U)
        dut.io.y.expect(E2M1.add(a, b).U)
      }
    }
  }

  "RTL multiplication matches all 256 reference pairs" in {
    simulate(new FP4Multiply) { dut =>
      for (a <- 0 until 16; b <- 0 until 16) {
        dut.io.a.poke(a.U)
        dut.io.b.poke(b.U)
        dut.io.y.expect(E2M1.multiply(a, b).U)
      }
    }
  }

  "RTL fused MAC matches all 4096 reference triples" in {
    simulate(new FP4FMA) { dut =>
      for (a <- 0 until 16; b <- 0 until 16; c <- 0 until 16) {
        dut.io.a.poke(a.U)
        dut.io.b.poke(b.U)
        dut.io.c.poke(c.U)
        dut.io.y.expect(E2M1.fma(a, b, c).U)
      }
    }
  }

  "RTL dot-product partial sums match directed and seeded random vectors" in {
    for (k <- Seq(1, 4, 8)) {
      simulate(new FP4DotProduct(k)) { dut =>
        val random = new scala.util.Random(20261026L + k)
        val directed = Seq(
          (Seq.fill(k)(0), Seq.fill(k)(7), 0),
          (Seq.fill(k)(7), Seq.tabulate(k)(i => if (i % 2 == 0) 7 else 15), 0),
          (Seq.fill(k)(1), Seq.fill(k)(2), 0),
          (Seq.fill(k)(8), Seq.fill(k)(15), 3)
        )
        val randomInputs = Seq.fill(200) {
          (Seq.fill(k)(random.nextInt(16)), Seq.fill(k)(random.nextInt(16)), random.nextInt(16))
        }
        for ((a, b, initial) <- directed ++ randomInputs) {
          dut.io.initial.poke(initial.U)
          var acc = initial
          for (i <- 0 until k) {
            dut.io.a(i).poke(a(i).U)
            dut.io.b(i).poke(b(i).U)
          }
          for (i <- 0 until k) {
            acc = E2M1.fma(a(i), b(i), acc)
            dut.io.partialSums(i).expect(acc.U)
          }
          dut.io.y.expect(acc.U)
        }
      }
    }
  }
}
