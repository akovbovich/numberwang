package example

object Main extends App {

  def run(a: Int, b: Int): Unit = {
    val res = LogLogSolution(2, 1_000_000_000).solution(a, b)
    println(res)
  }

  try run(args(0).toInt, args(1).toInt)
  catch {
    case _: Throwable =>
      System.err.println("invalid args")
      sys.exit(1)
  }
}
