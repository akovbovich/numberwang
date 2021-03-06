package example

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext

final case class MutableHashMapSolution(private val minInclusive: Int, private val maxInclusive: Int)(implicit
  protected val ec: ExecutionContext
) extends AsyncSolution(minInclusive, maxInclusive) {

  private val hashMap =
    mutable.HashMap.from(Iterable.tabulate(math.floor(math.sqrt(maxInclusive.toDouble)).toInt + 1)(i => i * i -> i))

  override protected def perfectSquareIterations(number: Int): Int = {
    @tailrec def loop(number: Int, count: Int): Int =
      hashMap.get(number) match {
        case None    => count
        case Some(x) => loop(x, count + 1)
      }

    loop(number, 0)
  }
}
