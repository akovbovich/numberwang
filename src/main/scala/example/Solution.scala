package example

import scala.collection.mutable

import scala.annotation.tailrec
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext

sealed abstract class Solution {
  protected val minInclusive: Int
  protected val maxInclusive: Int
  protected implicit val ec: ExecutionContext
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
