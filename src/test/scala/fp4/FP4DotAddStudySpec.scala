package tensorgauge.fp4

import org.scalatest.freespec.AnyFreeSpec

class FP4DotAddStudySpec extends AnyFreeSpec {
  "the normal dataset is frozen, encoded, and includes 10000 four-product samples" in {
    assert(FP4DotAddStudy.samples.length == 10000)
    assert(FP4DotAddStudy.samples.forall(s => s.a.length == 4 && s.b.length == 4))
    assert(FP4DotAddStudy.samples.forall(s => (s.a ++ s.b).forall(x => x >= 0 && x < 16)))
    assert(FP4DotAddStudy.inputCsv.linesIterator.length == 10001)
    assert(FP4DotAddStudy.inputSha256.matches("[0-9a-f]{64}"))
  }

  "narrow retention has error while seven fractional positions are lossless for C=0 FP4 products" in {
    assert(FP4DotAddStudy.statistics(0).mse > 0)
    val wide = FP4DotAddStudy.statistics(7)
    assert(wide.mse == 0.0 && wide.maxAbs == 0.0 && wide.mismatches == 0)
    assert(FP4DotAddStudy.statistics(160) == wide)
  }

  "the sweep records each configuration and common input fingerprint deterministically" in {
    val first = FP4DotAddStudy.summaryCsv
    assert(FP4DotAddStudy.summaryCsv == first)
    assert(first.linesIterator.length == FP4DotAddStudy.widths.length + 1)
    assert(first.linesIterator.drop(1).forall(_.endsWith(FP4DotAddStudy.inputSha256)))
    assert(!first.contains("NaN") && !first.contains("Infinity"))
  }
}
