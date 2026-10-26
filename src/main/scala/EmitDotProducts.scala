package tensorgauge

import chisel3._
import _root_.circt.stage.ChiselStage
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import tensorgauge.fp4._

object EmitDotProducts extends App {
  require(args.forall(_ == "--systemverilog"), "usage: EmitDotProducts [--systemverilog]")
  val systemVerilog = args.contains("--systemverilog")
  val root = Paths.get("generated")
  Files.createDirectories(root)

  def emit(name: String, generator: => RawModule): Unit = {
    val code = if (systemVerilog) ChiselStage.emitSystemVerilog(generator,
      firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", "-default-layer-specialization=enable"))
      else ChiselStage.emitCHIRRTL(generator)
    val extension = if (systemVerilog) "sv" else "fir"
    val destination = root.resolve(s"$name.$extension")
    Files.write(destination, code.getBytes(UTF_8))
    println(s"Emitted $destination")
  }

  Experiments.configs.foreach(c => emit(c.name, new DotProduct(c.k, c.width, c.intermediateWidth)))
  emit("FP4Decode", new FP4Decode)
  emit("FP4Add", new FP4Add)
  emit("FP4Multiply", new FP4Multiply)
  emit("FP4FMA", new FP4FMA)
  Seq(1, 4, 8).foreach(k => emit(s"FP4DotProduct_K$k", new FP4DotProduct(k)))
}
