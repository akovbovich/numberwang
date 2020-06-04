package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SolutionSpec extends AnyFlatSpec with Matchers {
  import scala.concurrent.ExecutionContext.Implicits.global

  def withMutableHashMapSolution(test: Solution => Any) = {
    val sol: Solution = new MutableHashMapSolution(2, 1_000_000_000)
    test(sol)
  }

  def withMathSqrtOptSolution(test: Solution => Any) = {
    val sol: Solution = new MathSqrtOptSolution(2, 1_000_000_000)
    test(sol)
  }

  "HashMapSolution" should "return correct answer" in withMutableHashMapSolution { s =>
    s.solution(10, 20) shouldEqual 2
    s.solution(6000, 7000) shouldEqual 3
  }

  it should "throw on invalid input" in withMutableHashMapSolution { s =>
    an[IllegalArgumentException] should be thrownBy s.solution(-1, 2)
    an[IllegalArgumentException] should be thrownBy s.solution(1, 2)
    an[IllegalArgumentException] should be thrownBy s.solution(2, 1)
    an[IllegalArgumentException] should be thrownBy s.solution(2, 1_000_000_001)
  }

  "MathSqrtOptSolution" should "return correct answer" in withMathSqrtOptSolution { s =>
    s.solution(10, 20) shouldEqual 2
    s.solution(6000, 7000) shouldEqual 3
  }

  it should "throw on invalid input" in withMathSqrtOptSolution { s =>
    an[IllegalArgumentException] should be thrownBy s.solution(-1, 2)
    an[IllegalArgumentException] should be thrownBy s.solution(1, 2)
    an[IllegalArgumentException] should be thrownBy s.solution(2, 1)
    an[IllegalArgumentException] should be thrownBy s.solution(2, 1_000_000_001)
  }
}
