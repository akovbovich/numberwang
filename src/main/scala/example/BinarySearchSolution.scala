package example

final case class BinarySearchSolution(private val minInclusive: Int, private val maxInclusive: Int) extends Solution {
  @throws[IllegalArgumentException]
  def solution(a: Int, b: Int): Int = {
    if (a < minInclusive || b > maxInclusive || !(a <= b)) throw new IllegalArgumentException("invalid bounds")
    loop(math.ceil(isqrt(a)).toInt, math.floor(isqrt(b)).toInt, 0)
  }

  private def loop(lhs: Int, rhs: Int, count: Int): Int =
    if (lhs <= rhs)
      loop(math.ceil(isqrt(lhs)).toInt, math.floor(isqrt(rhs)).toInt, count + 1)
    else
      count

  private def isqrt(n: Int): Double = math.sqrt(n.toDouble)
}
