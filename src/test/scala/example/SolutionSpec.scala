package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionSpec extends AnyFlatSpec with Matchers {
  implicit val ec                      = scala.concurrent.ExecutionContext.global
  val mutableHashMapSolution: Solution = new MutableHashMapSolution(2, 1_000_000_000)
  val mathSqrtOptSolution: Solution    = new MathSqrtOptSolution(2, 1_000_000_000)

  def withSolutions(solutions: Solution*)(test: Solution => Any) =
    solutions.foreach { s =>
      note(s"Now checking solution $s")
      test(s)
    }

  "Solution" should "return correct answer" in withSolutions(mutableHashMapSolution, mathSqrtOptSolution) { s =>
    s.solution(10, 20) shouldEqual 2
    s.solution(6000, 7000) shouldEqual 3
  }

  it should "throw on invalid input" in withSolutions(mutableHashMapSolution, mathSqrtOptSolution) { s =>
    an[IllegalArgumentException] should be thrownBy s.solution(-1, 2)
    an[IllegalArgumentException] should be thrownBy s.solution(1, 2)
    an[IllegalArgumentException] should be thrownBy s.solution(2, 1)
    an[IllegalArgumentException] should be thrownBy s.solution(2, 1_000_000_001)
  }
}
