package example

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

final case class MathSqrtOptSolution(minInclusive: Int, maxInclusive: Int)(implicit val ec: ExecutionContext)
    extends AsyncSolution(minInclusive, maxInclusive) {

  override protected def perfectSquareIterations(number: Int): Int = {
    @tailrec def loop(number: Int, count: Int): Int =
      (number & 0xf) match {
        case 0 | 1 | 4 | 9 =>
          val test = math.sqrt(number.toDouble).toInt
          if (test * test != number) count else loop(test, count + 1)
        case _ => count
      }

    loop(number, 0)
  }
}
