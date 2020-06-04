package example

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext

abstract class Solution(minInclusive: Int, maxInclusive: Int) {
  protected def perfectSquareIterations(number: Int): Int

  private def maxPerfectSquareIterationsInRange(range: Range): Int =
    range.foldLeft(0)((m, i) => math.max(m, perfectSquareIterations(i)))

  @throws[IllegalArgumentException]
  final def solution(a: Int, b: Int)(implicit ec: ExecutionContext): Int = {
    if (a < minInclusive || b > maxInclusive || !(a <= b)) throw new IllegalArgumentException("invalid bounds")
    val range       = a to b
    val numCores    = Runtime.getRuntime().availableProcessors()
    val partSize    = range.knownSize / numCores
    val minPartSize = 32 * 1024
    Await.result(
      Future
        .traverse(range.grouped(math.max(minPartSize, partSize)))(part =>
          Future(maxPerfectSquareIterationsInRange(part))
        )
        .map(_.max),
      Duration.Inf
    )
  }
}
