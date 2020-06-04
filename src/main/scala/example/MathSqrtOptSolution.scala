package example

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

final case class MathSqrtOptSolution(val minInclusive: Int, val maxInclusive: Int)(implicit val ec: ExecutionContext)
    extends Solution {
  override protected def maxPerfectSquareIterations(range: Range): Int =
    range.foldLeft(0)((m, i) => math.max(m, perfectSquareIterations(i)))

  @tailrec
  private def perfectSquareIterations(number: Int, count: Int = 0): Int =
    (number & 0xf) match {
      case 0 | 1 | 4 | 9 =>
        val test = math.sqrt(number.toDouble).toInt
        if (test * test != number) count else perfectSquareIterations(test, count + 1)
      case _ => count
    }
}
