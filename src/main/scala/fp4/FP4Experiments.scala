package tensorgauge.fp4

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

final case class FP4Input(label: String, a: Seq[Double], b: Seq[Double]) {
  require(a.nonEmpty && a.length == b.length)
}

object FP4Experiments {
  val inputs: Seq[FP4Input] = Seq(
    FP4Input("zero", Seq(0, 0, 0, 0), Seq(6, -6, 1, -1)),
    FP4Input("exact_small", Seq(1, 0.5, -1, 0.5), Seq(1, 1, 1, 1)),
    FP4Input("accumulation_stagnation", Seq.fill(8)(0.5), Seq.fill(8)(1.0)),
    FP4Input("saturation_then_cancellation", Seq(6, 6), Seq(6, -6)),
    FP4Input("conversion_error", Seq(0.3, 1.2, -2.7, 0.8), Seq(1.1, -0.6, 0.4, 1.9))
  )
}

/** Software experiment only: none of these rows are RTL measurements. */
object RunFP4Experiments extends App {
  val output = Paths.get("reports", "fp4-baseline.csv")
  Files.createDirectories(output.getParent)
  val header = "case,k,original_dot,encoded_exact_dot,fp4_result,output_code,conversion_abs_error,accumulation_abs_error,total_abs_error"
  val rows = FP4Experiments.inputs.map { input =>
    val a = input.a.map(E2M1.fromDouble)
    val b = input.b.map(E2M1.fromDouble)
    val original = input.a.zip(input.b).map { case (x, y) => BigDecimal(x) * BigDecimal(y) }.sum
    val exact = E2M1.exactDot(a, b)
    val code = E2M1.dot(a, b)
    val result = E2M1.decode(code)
    println(s"${input.label}: exact(encoded)=$exact, FP4=$result, code=0x${code.toHexString}")
    Seq(input.label, a.length, original, exact, result, s"0x${code.toHexString}",
      (exact - original).abs, (result - exact).abs, (result - original).abs).mkString(",")
  }
  Files.write(output, (header +: rows).mkString("", "\n", "\n").getBytes(UTF_8))
  println(s"Wrote $output (software reference results).")
}
