package tensorgauge.fp4

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import java.util.Locale

/** A small numerical study of the existing fixed, unscaled E2M1 baseline.
  * Decimal input generation makes both unrounded reference calculations exact
  * for this dataset. Error statistics are software results; the separate RTL
  * suite replays every sample to check hardware agreement.
  */
object FP4GemmEvaluation {
  type DecimalMatrix = Vector[Vector[BigDecimal]]
  final case class Shape(m: Int, n: Int, k: Int) {
    require(m > 0 && n > 0 && k > 0)
  }
  final case class Sample(pattern: String, shape: Shape, trial: Int, seed: Long,
                          a: DecimalMatrix, b: DecimalMatrix, c: DecimalMatrix)
  final case class Metrics(maxAbs: BigDecimal, relativeL2: Option[Double])
  final case class Result(sample: Sample, original: DecimalMatrix,
                          encodedExact: DecimalMatrix, fp4: FP4GemmReference.Matrix)

  val shapes: Vector[Shape] = Vector(Shape(1, 1, 2), Shape(2, 2, 4), Shape(2, 3, 8))
  val patterns: Vector[String] = Vector("exact_inputs", "uniform", "cancellation", "large_range")
  val trials = 10
  val baseSeed = 20261116L

  def encode(matrix: DecimalMatrix): FP4GemmReference.Matrix = matrix.map(_.map(E2M1.encode))
  def decode(matrix: FP4GemmReference.Matrix): DecimalMatrix = matrix.map(_.map(E2M1.decode))

  private def validate(name: String, matrix: DecimalMatrix, rows: Int, columns: Int): Unit =
    require(matrix.length == rows && matrix.forall(_.length == columns), s"$name shape mismatch")

  def exactGemm(shape: Shape, a: DecimalMatrix, b: DecimalMatrix, c: DecimalMatrix): DecimalMatrix = {
    validate("A", a, shape.m, shape.k)
    validate("B", b, shape.k, shape.n)
    validate("C", c, shape.m, shape.n)
    Vector.tabulate(shape.m, shape.n) { (i, j) =>
      (0 until shape.k).foldLeft(c(i)(j))((sum, p) => sum + a(i)(p) * b(p)(j))
    }
  }

  private def generate(pattern: String, s: Shape, trial: Int, seed: Long): Sample = {
    val random = new scala.util.Random(seed)
    val small = Vector("-1", "-0.5", "0", "0.5", "1").map(BigDecimal(_))
    def exact(rows: Int, columns: Int): DecimalMatrix =
      Vector.fill(rows, columns)(small(random.nextInt(small.length)))
    def uniform(rows: Int, columns: Int, bound: Int): DecimalMatrix =
      Vector.fill(rows, columns)(BigDecimal(random.nextInt(2 * bound + 1) - bound) / 100)
    val (a, b, c) = pattern match {
      case "exact_inputs" => (exact(s.m, s.k), exact(s.k, s.n), exact(s.m, s.n))
      case "uniform" => (uniform(s.m, s.k, 200), uniform(s.k, s.n, 200), uniform(s.m, s.n, 100))
      case "large_range" => (uniform(s.m, s.k, 1200), uniform(s.k, s.n, 1200), uniform(s.m, s.n, 200))
      case "cancellation" =>
        require(s.k % 2 == 0, "cancellation samples require an even K")
        val magnitudes = Vector("0.5", "1", "1.5", "2", "3", "4", "6").map(BigDecimal(_))
        val aPairs = Vector.fill(s.m, s.k / 2)(magnitudes(random.nextInt(magnitudes.length)))
        val bPairs = Vector.fill(s.k / 2, s.n)(magnitudes(random.nextInt(magnitudes.length)))
        val pairedA = Vector.tabulate(s.m, s.k)((i, p) => aPairs(i)(p / 2))
        val pairedB = Vector.tabulate(s.k, s.n)((p, j) =>
          if (p % 2 == 0) bPairs(p / 2)(j) else -bPairs(p / 2)(j))
        (pairedA, pairedB, Vector.fill(s.m, s.n)(BigDecimal(0)))
      case other => throw new IllegalArgumentException(s"unknown pattern: $other")
    }
    Sample(pattern, s, trial, seed, a, b, c)
  }

  lazy val samples: Vector[Sample] = for {
    s <- shapes
    (pattern, index) <- patterns.zipWithIndex
    trial <- 0 until trials
    seed = baseSeed + index * 1000000L + s.m * 10000L + s.n * 1000L + s.k * 100L + trial
  } yield generate(pattern, s, trial, seed)

  def evaluate(sample: Sample): Result = {
    val a = encode(sample.a)
    val b = encode(sample.b)
    val c = encode(sample.c)
    Result(sample,
      exactGemm(sample.shape, sample.a, sample.b, sample.c),
      exactGemm(sample.shape, decode(a), decode(b), decode(c)),
      FP4GemmReference.gemm(a, b, c))
  }

  /** For matrices, flattened L2 is the Frobenius norm. Relative error is
    * undefined when the reference is entirely zero; absolute error still applies.
    */
  def metrics(reference: Seq[BigDecimal], observed: Seq[BigDecimal]): Metrics = {
    require(reference.nonEmpty && reference.length == observed.length)
    val errors = reference.zip(observed).map { case (x, y) => y - x }
    val denominator = reference.map(x => x * x).sum
    val numerator = errors.map(x => x * x).sum
    Metrics(errors.map(_.abs).max,
      if (denominator == 0) None else Some(math.sqrt((numerator / denominator).toDouble)))
  }

  private def decimal(x: BigDecimal): String = x.bigDecimal.stripTrailingZeros.toPlainString
  private def relative(x: Option[Double]): String = x
    .map(value => java.lang.String.format(Locale.ROOT, "%.8g", Double.box(value))).getOrElse("NA")

  /** Returns deterministic CSV text; no timestamps or host-dependent paths. */
  def reports(results: Vector[Result]): (String, String) = {
    require(results.nonEmpty)
    val elementsHeader = "pattern,m,n,k,trial,seed,row,column,original,encoded_exact,fp4,quantization_error,accumulation_error,total_error"
    val elementRows = results.flatMap { result =>
      val sample = result.sample
      val s = sample.shape
      for (i <- 0 until s.m; j <- 0 until s.n) yield {
        val original = result.original(i)(j)
        val encoded = result.encodedExact(i)(j)
        val actual = E2M1.decode(result.fp4(i)(j))
        (Seq(sample.pattern, s.m, s.n, s.k, sample.trial, sample.seed, i, j) ++
          Seq(original, encoded, actual, encoded - original, actual - encoded, actual - original).map(decimal))
          .mkString(",")
      }
    }
    val summaryHeader = "pattern,m,n,k,samples,elements,quantization_max_abs,quantization_relative_l2,accumulation_max_abs,accumulation_relative_l2,total_max_abs,total_relative_l2"
    val summaryRows = shapes.flatMap { s => patterns.flatMap { pattern =>
      val group = results.filter(r => r.sample.shape == s && r.sample.pattern == pattern)
      if (group.isEmpty) None else {
        val original = group.flatMap(_.original.flatten)
        val encoded = group.flatMap(_.encodedExact.flatten)
        val actual = group.flatMap(r => decode(r.fp4).flatten)
        val quantization = metrics(original, encoded)
        val accumulation = metrics(encoded, actual)
        val total = metrics(original, actual)
        Some(Seq(pattern, s.m, s.n, s.k, group.length, original.length,
          decimal(quantization.maxAbs), relative(quantization.relativeL2),
          decimal(accumulation.maxAbs), relative(accumulation.relativeL2),
          decimal(total.maxAbs), relative(total.relativeL2)).mkString(","))
      }
    }}
    ((elementsHeader +: elementRows).mkString("", "\n", "\n"),
      (summaryHeader +: summaryRows).mkString("", "\n", "\n"))
  }
}

object RunFP4GemmEvaluation extends App {
  val results = FP4GemmEvaluation.samples.map(FP4GemmEvaluation.evaluate)
  val (elements, summary) = FP4GemmEvaluation.reports(results)
  val directory = Paths.get("reports")
  Files.createDirectories(directory)
  Files.write(directory.resolve("fp4-gemm-evaluation-elements.csv"), elements.getBytes(UTF_8))
  Files.write(directory.resolve("fp4-gemm-evaluation-summary.csv"), summary.getBytes(UTF_8))
  println("FP4 GEMM numerical evaluation (software reference; RTL agreement checked separately)")
  println(s"${results.length} matrices, seed ${FP4GemmEvaluation.baseSeed}; C-seeded, unscaled E2M1 MACs")
  println(summary)
  println("NA: relative error has an all-zero reference; inspect absolute error instead.")
  println("Wrote reports/fp4-gemm-evaluation-{elements,summary}.csv")
}
