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
}
