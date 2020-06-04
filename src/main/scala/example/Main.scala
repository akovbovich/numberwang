package example

object Main extends App {
  val res =
    try {
      val (a, b) = (args(0).toInt, args(1).toInt)
      LogLogSolution(2, 1_000_000_000).solution(a, b)
    } catch {
      case _: Throwable =>
        println("invalid args")
        sys.exit(1)
    }

  println(res)
}
