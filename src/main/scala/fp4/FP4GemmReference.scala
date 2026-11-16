package tensorgauge.fp4

/** Matrix-level software oracle using the independently implemented E2M1 FMA.
  * Matrix entries are four-bit encodings (integers 0..15), not host values.
  */
object FP4GemmReference {
  type Matrix = Vector[Vector[Int]]

  private def shape(name: String, matrix: Seq[Seq[Int]]): (Int, Int) = {
    require(matrix.nonEmpty && matrix.head.nonEmpty, s"$name must be nonempty")
    val columns = matrix.head.length
    require(matrix.forall(_.length == columns), s"$name must be rectangular")
    require(matrix.iterator.flatten.forall(x => x >= 0 && x < 16),
      s"$name entries must be E2M1 encodings in 0..15")
    (matrix.length, columns)
  }

  /** D = A*B+C with C seeded BEFORE the ordered, per-lane rounded MACs.
    * This is not equivalent to computing a rounded A*B and adding C afterward.
    */
  def gemm(a: Seq[Seq[Int]], b: Seq[Seq[Int]], c: Seq[Seq[Int]]): Matrix = {
    val (m, k) = shape("A", a)
    val (bRows, n) = shape("B", b)
    val cShape = shape("C", c)
    require(bRows == k, "A columns must equal B rows")
    require(cShape == (m, n), "C must match the M x N output shape")
    Vector.tabulate(m, n) { (row, column) =>
      (0 until k).foldLeft(c(row)(column)) { (acc, inner) =>
        E2M1.fma(a(row)(inner), b(inner)(column), acc)
      }
    }
  }

  def multiply(a: Seq[Seq[Int]], b: Seq[Seq[Int]]): Matrix = {
    val (m, _) = shape("A", a)
    val (_, n) = shape("B", b)
    gemm(a, b, Vector.fill(m, n)(E2M1.positiveZero))
  }
}

/** Small functional demonstration; this is software output, not RTL evidence. */
object RunFP4Gemm extends App {
  def encode(rows: Seq[Seq[Double]]): FP4GemmReference.Matrix =
    rows.map(_.map(E2M1.fromDouble).toVector).toVector
  val a = encode(Seq(Seq(1, 0.5, -1), Seq(0, 1, 0.5)))
  val b = encode(Seq(Seq(1, -1), Seq(1, 1), Seq(0.5, 1)))
  val c = encode(Seq(Seq(0.5, 0), Seq(-0.5, 1)))
  val result = FP4GemmReference.gemm(a, b, c)
  println("FP4 GEMM: A(2x3) * B(3x2) + C(2x2), C-seeded ordered MACs")
  result.foreach(row => println(row.map(E2M1.decode).mkString("[", ", ", "]")))
}
