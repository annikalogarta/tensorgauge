package tensorgauge.fp4

import org.scalatest.freespec.AnyFreeSpec

class FP4GemmEvaluationSpec extends AnyFreeSpec {
  import FP4GemmEvaluation._
  private def matrix(rows: Seq[Seq[String]]): DecimalMatrix =
    rows.map(_.map(BigDecimal(_)).toVector).toVector

  "error metrics retain absolute error and leave zero-reference relative error undefined" in {
    val ordinary = metrics(Seq(BigDecimal(3), BigDecimal(4)), Seq(BigDecimal(0), BigDecimal(0)))
    assert(ordinary.maxAbs == 4)
    assert(ordinary.relativeL2.contains(1.0))
    val zeroReference = metrics(Seq(BigDecimal(0), BigDecimal(0)), Seq(BigDecimal(1), BigDecimal(-1)))
    assert(zeroReference.maxAbs == 1)
    assert(zeroReference.relativeL2.isEmpty)
    intercept[IllegalArgumentException](metrics(Seq.empty, Seq.empty))
    intercept[IllegalArgumentException](metrics(Seq(BigDecimal(1)), Seq.empty))
  }

  "a known C-seeded example isolates accumulation error" in {
    val sample = Sample("exact_inputs", Shape(1, 1, 2), 0, 0,
      matrix(Seq(Seq("0.5", "0.5"))), matrix(Seq(Seq("1"), Seq("1"))), matrix(Seq(Seq("2"))))
    val result = evaluate(sample)
    assert(result.original == matrix(Seq(Seq("3"))))
    assert(result.encodedExact == result.original)
    assert(decode(result.fp4) == matrix(Seq(Seq("2"))))
  }

  "a known conversion example isolates quantization error" in {
    val sample = Sample("uniform", Shape(1, 1, 2), 0, 0,
      matrix(Seq(Seq("0.3", "0"))), matrix(Seq(Seq("1"), Seq("1"))), matrix(Seq(Seq("0.1"))))
    val result = evaluate(sample)
    assert(result.original == matrix(Seq(Seq("0.4"))))
    assert(result.encodedExact == matrix(Seq(Seq("0.5"))))
    assert(decode(result.fp4) == result.encodedExact)
  }

  "out-of-range source values contribute to input quantization error" in {
    val sample = Sample("large_range", Shape(1, 1, 2), 0, 0,
      matrix(Seq(Seq("12", "0"))), matrix(Seq(Seq("1"), Seq("0"))), matrix(Seq(Seq("0"))))
    val result = evaluate(sample)
    assert(result.original == matrix(Seq(Seq("12"))))
    assert(result.encodedExact == matrix(Seq(Seq("6"))))
    assert(decode(result.fp4) == result.encodedExact)
  }

  "representable and cancellation datasets have no input-conversion error" in {
    val exact = samples.filter(s => s.pattern == "exact_inputs" || s.pattern == "cancellation").map(evaluate)
    assert(exact.forall(r => r.original == r.encodedExact))
    val cancellation = exact.filter(_.sample.pattern == "cancellation")
    assert(cancellation.forall(_.original.flatten.forall(_ == 0)))
    assert(cancellation.exists(r => decode(r.fp4).flatten.exists(_ != 0)))
  }

  "reports are deterministic, keep signed error decomposition, and avoid NaN/Infinity" in {
    val results = samples.map(evaluate)
    assert(results.length == 120)
    assert(samples.map(_.seed).distinct.length == samples.length)
    for (r <- results; i <- 0 until r.sample.shape.m; j <- 0 until r.sample.shape.n) {
      val quantization = r.encodedExact(i)(j) - r.original(i)(j)
      val accumulation = E2M1.decode(r.fp4(i)(j)) - r.encodedExact(i)(j)
      assert(quantization + accumulation == E2M1.decode(r.fp4(i)(j)) - r.original(i)(j))
    }
    val (elements, summary) = reports(results)
    assert(reports(samples.map(evaluate)) == ((elements, summary)))
    assert(elements.linesIterator.length == 441)
    assert(summary.linesIterator.length == 13)
    assert(summary.linesIterator.filter(_.startsWith("cancellation,")).forall(_.endsWith(",NA")))
    assert(!summary.contains("NaN") && !summary.contains("Infinity"))
  }
}
