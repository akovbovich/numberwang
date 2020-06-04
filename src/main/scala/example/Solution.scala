package example

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext

abstract class Solution {
  protected implicit val ec: ExecutionContext

  protected val minInclusive: Int
  protected val maxInclusive: Int

  protected def maxPerfectSquareIterations(range: Range): Int

  @throws[IllegalArgumentException]
  final def solution(a: Int, b: Int): Int = {
    if (a < minInclusive || b > maxInclusive || !(a <= b)) throw new IllegalArgumentException("invalid bounds")
    val range      = a to b
    val numCores   = Runtime.getRuntime().availableProcessors()
    val rangeCount = range.knownSize / numCores
    val minSlice   = 32 * 1024
    val rangeParts = if (rangeCount > 0) range.grouped(math.max(rangeCount, minSlice)) else Iterator(range)
    Await.result(Future.traverse(rangeParts)(part => Future(maxPerfectSquareIterations(part))).map(_.max), Duration.Inf)
  }
}
