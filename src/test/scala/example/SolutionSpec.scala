package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionSpec extends AnyFlatSpec with Matchers {
  implicit val ec            = scala.concurrent.ExecutionContext.global
  val mutableHashMapSolution = MutableHashMapSolution(2, 1_000_000_000)
  val mathSqrtOptSolution    = MathSqrtOptSolution(2, 1_000_000_000)
  val logLogSolution         = LogLogSolution(2, 1_000_000_000)

  def withSolutions(solutions: Solution*)(test: Solution => Any) =
    solutions.foreach { s =>
      note(s"Now checking solution $s")
      test(s)
    }

  "Solution" should "return correct answer" in withSolutions(
    mutableHashMapSolution,
    mathSqrtOptSolution,
    logLogSolution
  ) { s =>
    s.solution(10, 20) shouldEqual 2
    s.solution(6000, 7000) shouldEqual 3
  }

  it should "throw on invalid input" in withSolutions(mutableHashMapSolution, mathSqrtOptSolution, logLogSolution) {
    s =>
      an[IllegalArgumentException] should be thrownBy s.solution(-1, 2)
      an[IllegalArgumentException] should be thrownBy s.solution(1, 2)
      an[IllegalArgumentException] should be thrownBy s.solution(2, 1)
      an[IllegalArgumentException] should be thrownBy s.solution(2, 1_000_000_001)
  }
}
