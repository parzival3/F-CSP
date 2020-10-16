package ac

import org.scalatest.flatspec.AnyFlatSpec

class TestSolver extends AnyFlatSpec {
  behavior of "Solver"

  it should "be able to get a solution" in {
    val var1 = Variable("a")
    val var2 = Variable("b")
    val c1 = Constraint((a: Int, b: Int) => a + 1 == b)
    val directGraph: Map[Constraint, (Variable, Variable)] = Map((c1 -> ((var1, var2))))


    val listOfDomains = Map(Variable("a") -> Domain((0 to 10).toList), Variable("b") -> Domain((0 to 10).toList))
    val csp = new CSP(List(var1,var2), listOfDomains, directGraph)
    val solution = Solution(csp, Assignments(Map(Variable("a") -> 0)))
    print(solution.backtrack(solution).head)
  }
}
