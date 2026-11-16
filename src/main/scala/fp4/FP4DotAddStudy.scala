package tensorgauge.fp4

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import java.security.MessageDigest
import java.util.Locale

/** Figure-6a-inspired operator study; this is an unscaled E2M1 adaptation,
  * not a reproduction of the paper's NVFP4/MXFP4 numerical curves.
  */
object FP4DotAddStudy {
  final case class Sample(a: Vector[Int], b: Vector[Int], reference: Long)
  final case class Statistics(mse: Double, maxAbs: Double, mismatches: Int)
  val k = 4
  val count = 10000
  val seed = 20261116L
  val widths = Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 12, 16, 24, 160)
  val c = 0L

  lazy val samples: Vector[Sample] = {
    val random = new scala.util.Random(seed)
    Vector.fill(count) {
      val a = Vector.fill(k)(E2M1.fromDouble(random.nextGaussian()))
      val b = Vector.fill(k)(E2M1.fromDouble(random.nextGaussian()))
      Sample(a, b, FP4DotAddReference.exact(a, b, c))
    }
  }

  /** All rows use these same encoded operands. Values are codes, not floats. */
  lazy val inputCsv: String = {
    val header = (Seq("sample") ++ (0 until k).map(i => s"a$i") ++
      (0 until k).map(i => s"b$i") ++ Seq("c_bits", "reference_bits")).mkString(",")
    val rows = samples.zipWithIndex.map { case (sample, index) =>
      (Seq(index) ++ sample.a ++ sample.b ++ Seq(f"0x$c%08x", f"0x${sample.reference}%08x")).mkString(",")
    }
    (header +: rows).mkString("", "\n", "\n")
  }

  lazy val inputSha256: String = MessageDigest.getInstance("SHA-256")
    .digest(inputCsv.getBytes(UTF_8)).map(byte => f"${byte & 255}%02x").mkString

  def statistics(p: Int): Statistics = {
    var squaredError = 0.0
    var maxAbs = 0.0
    var mismatches = 0
    for (sample <- samples) {
      val actual = FP4DotAddReference(sample.a, sample.b, c, p)
      val difference = Binary32.toFloat(actual).toDouble - Binary32.toFloat(sample.reference).toDouble
      squaredError += difference * difference
      maxAbs = math.max(maxAbs, math.abs(difference))
      if (actual != sample.reference) mismatches += 1
    }
    Statistics(squaredError / samples.length, maxAbs, mismatches)
  }

  private def number(value: Double): String =
    java.lang.String.format(Locale.ROOT, "%.12g", Double.box(value))

  def summaryCsv: String = {
    val header = "frac_bits,k,samples,seed,c_bits,input_distribution,alignment_discard,output_format,mse,max_abs_error,mismatches,input_sha256"
    val rows = widths.map { p =>
      val result = statistics(p)
      Seq(p, k, count, seed, "0x00000000", "normal_0_1_then_e2m1", "toward_zero", "binary32_rne",
        number(result.mse), number(result.maxAbs), result.mismatches, inputSha256).mkString(",")
    }
    (header +: rows).mkString("", "\n", "\n")
  }
}

object RunFP4DotAddStudy extends App {
  val directory = Paths.get("reports")
  Files.createDirectories(directory)
  val summary = FP4DotAddStudy.summaryCsv
  Files.write(directory.resolve("fp4-dot-add-inputs.csv"), FP4DotAddStudy.inputCsv.getBytes(UTF_8))
  Files.write(directory.resolve("fp4-dot-add-sweep.csv"), summary.getBytes(UTF_8))
  println("Unscaled E2M1 FDA study: K=4, FP32 C/output, magnitude truncation, final RNE")
  println("Software-reference statistics; the RTL study test replays these same inputs and widths.")
  println("MSE reference: exact sum over the same quantized operands, rounded once to binary32.")
  println(summary)
  println("This is a local Figure-6a-inspired adaptation; paper-specific scaling/settings differ.")
}
