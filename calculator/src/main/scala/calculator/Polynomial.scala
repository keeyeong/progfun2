package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = Signal {
    // Δ = b² - 4ac
    val B = b()
    B * B - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    // (-b ± √Δ) / 2a
    if (b() < 0) Set.empty
    else {
      val pos = (-b() + delta()) / (2 * a())
      val neg = (-b() - delta()) / (2 * a())
      Set(pos, neg)
    }
  }
}
