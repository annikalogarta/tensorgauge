package tensorgauge.fp4

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec

/** Several elaboration-time precision choices driven with identical inputs. */
private[fp4] class FP4DotAddBank(val k: Int, val widths: Seq[Int]) extends Module {
  val io = IO(new Bundle {
    val a = Input(Vec(k, UInt(4.W)))
    val b = Input(Vec(k, UInt(4.W)))
    val c = Input(UInt(32.W))
    val y = Output(Vec(widths.length, UInt(32.W)))
  })
  for ((p, index) <- widths.zipWithIndex) {
    val unit = Module(new FP4DotAdd(k, p))
    unit.io.a := io.a
    unit.io.b := io.b
    unit.io.c := io.c
    io.y(index) := unit.io.y
  }
}

class FP4DotAddRTLSpec extends AnyFreeSpec with ChiselSim {
  private def check(dut: FP4DotAddBank, a: Seq[Int], b: Seq[Int], c: Long): Unit = {
    for (i <- 0 until dut.k) {
      dut.io.a(i).poke(a(i).U)
      dut.io.b(i).poke(b(i).U)
    }
    dut.io.c.poke(c.U(32.W))
    for ((p, index) <- dut.widths.zipWithIndex) {
      val wanted = FP4DotAddReference(a, b, c, p)
      withClue(s"K=${dut.k},p=$p,a=$a,b=$b,C=0x${c.toHexString}: ") {
        dut.io.y(index).expect(wanted.U(32.W))
      }
    }
  }

  "scalar dot-add matches every FP4 pair with directed FP32 C at four internal widths" in {
    simulate(new FP4DotAddBank(1, Seq(0, 4, 24, 160))) { dut =>
      val cValues = Seq(0L, 0x80000000L, 1L, 0x80000001L, 0x007fffffL, 0x00800000L,
        0x3f000000L, 0xbf000000L, 0x42100000L, 0xc2100000L, 0x4b800000L,
        0x4b800001L, 0x7f7fffffL, 0xff7fffffL, 0x7f800000L, 0xff800000L, 0x7fa12345L)
      for (a <- 0 until 16; b <- 0 until 16; c <- cValues) check(dut, Seq(a), Seq(b), c)
    }
  }

  "four-product FDA matches directed cancellation and random full-range C at five widths" in {
    simulate(new FP4DotAddBank(4, Seq(0, 3, 7, 24, 160))) { dut =>
      val cases = Seq(
        (Seq(7, 1, 15, 0), Seq(7, 1, 7, 0), 0L),
        (Seq(7, 1, 0, 0), Seq(7, 1, 0, 0), Binary32.fromFloat(-36.0f)),
        (Seq(7, 15, 0, 0), Seq(7, 7, 0, 0), 1L),
        (Seq.fill(4)(7), Seq.fill(4)(7), 0L),
        (Seq.fill(4)(15), Seq.fill(4)(7), 0L),
        (Seq.fill(4)(0), Seq.fill(4)(0), 1L),
        (Seq.fill(4)(8), Seq.fill(4)(8), 0x80000001L),
        (Seq(2, 0, 0, 0), Seq(2, 0, 0, 0), 0x4b800001L)
      )
      cases.foreach { case (a, b, c) => check(dut, a, b, c) }
      val random = new scala.util.Random(20261120L)
      for (_ <- 0 until 500) {
        check(dut, Seq.fill(4)(random.nextInt(16)), Seq.fill(4)(random.nextInt(16)),
          random.nextInt().toLong & 0xffffffffL)
      }
    }
  }

  "reduction carry headroom preserves eight maximum products and C" in {
    simulate(new FP4DotAddBank(8, Seq(0, 7, 24, 160))) { dut =>
      for (a <- Seq(Seq.fill(8)(7), Seq.fill(8)(15));
           c <- Seq(0.0f, 288.0f, -288.0f, 0.25f)) {
        check(dut, a, Seq.fill(8)(7), Binary32.fromFloat(c))
      }
    }
  }

  "all 10000 frozen normal samples match RTL at every study width" in {
    simulate(new FP4DotAddBank(FP4DotAddStudy.k, FP4DotAddStudy.widths)) { dut =>
      for (sample <- FP4DotAddStudy.samples) check(dut, sample.a, sample.b, FP4DotAddStudy.c)
    }
  }
}
