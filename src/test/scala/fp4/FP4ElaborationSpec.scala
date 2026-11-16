package tensorgauge.fp4

import _root_.circt.stage.ChiselStage
import org.scalatest.freespec.AnyFreeSpec

/** Elaboration catches construction and connection failures; it does not execute
  * the RTL or establish bit-level equivalence with the reference model.
  */
class FP4ElaborationSpec extends AnyFreeSpec {
  "decoder and scalar arithmetic elaborate" in {
    assert(ChiselStage.emitCHIRRTL(new FP4Decode).contains("module FP4Decode"))
    assert(ChiselStage.emitCHIRRTL(new FP4Add).contains("module FP4Add"))
    assert(ChiselStage.emitCHIRRTL(new FP4Multiply).contains("module FP4Multiply"))
    assert(ChiselStage.emitCHIRRTL(new FP4FMA).contains("module FP4FMA"))
  }

  "dot products elaborate at one, four, and eight lanes" in {
    Seq(1, 4, 8).foreach { k =>
      val ir = ChiselStage.emitCHIRRTL(new FP4DotProduct(k))
      assert(ir.contains(s"module FP4DotProduct_K$k"))
    }
  }

  "zero-lane dot products are rejected" in {
    intercept[IllegalArgumentException](ChiselStage.emitCHIRRTL(new FP4DotProduct(0)))
  }

  "GEMM elaborates for scalar, rectangular, and matrix-vector shapes" in {
    Seq((1, 1, 1), (2, 2, 3), (2, 3, 4), (3, 1, 2), (1, 3, 2)).foreach {
      case (m, n, k) =>
        val ir = ChiselStage.emitCHIRRTL(new FP4Gemm(m, n, k))
        assert(ir.contains(s"module FP4Gemm_M${m}_N${n}_K$k"))
    }
  }

  "nonpositive GEMM dimensions are rejected" in {
    Seq((0, 1, 1), (1, 0, 1), (1, 1, 0), (-1, 2, 3)).foreach { case (m, n, k) =>
      intercept[IllegalArgumentException](ChiselStage.emitCHIRRTL(new FP4Gemm(m, n, k)))
    }
  }

  "mixed-precision dot-add elaborates at narrow and full-retention widths" in {
    Seq((1, 0), (4, 7), (8, 24), (4, 160)).foreach { case (k, p) =>
      assert(ChiselStage.emitCHIRRTL(new FP4DotAdd(k, p)).contains(s"module FP4DotAdd_K${k}_F$p"))
    }
  }

  "unsupported dot-add dimensions and precision are rejected" in {
    Seq((0, 0), (65, 0), (4, -1), (4, 161)).foreach { case (k, p) =>
      intercept[IllegalArgumentException](ChiselStage.emitCHIRRTL(new FP4DotAdd(k, p)))
    }
  }
}
