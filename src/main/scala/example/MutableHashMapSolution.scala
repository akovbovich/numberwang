package example

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext

final case class MutableHashMapSolution(val minInclusive: Int, val maxInclusive: Int)(implicit val ec: ExecutionContext)
    extends Solution {
  private val hashMap =
    mutable.HashMap.from(Iterable.tabulate(math.floor(math.sqrt(maxInclusive.toDouble)).toInt + 1)(i => i * i -> i))

  override protected def maxPerfectSquareIterations(range: Range): Int =
    range.foldLeft(0)((m, i) => math.max(m, perfectSquareIterations(i)))

  @tailrec
  private def perfectSquareIterations(number: Int, count: Int = 0): Int =
    hashMap.get(number) match {
      case None    => count
      case Some(x) => perfectSquareIterations(x, count + 1)
    }
}
