object Analyzer {
  val a = 0.081e-4
  val b = 0.183e-3

  case class Diff(value: Double, diff: Double)

  /*
   * t の 0.1% の刻みで t0　を取り, t0 が満たす方程式の右辺と左辺の差の絶対値が最小となる t0 の値を返す
   */
  def analyze(nb: Double, t: Double, tp: Double): Double = {
    val n = nb + t

    def diff(t0: Double): Diff =
      Diff(t0, Math.abs(t0 - (t + n * (1 + b * (t0 - tp) / (1 + 3 * a * (t0 - tp)))) + n))

    val diffs = for (i <- 100 to 0 by -1) yield diff(t + (t * i / 1000.0))

    diffs.minBy(_.diff).value
  }
}
