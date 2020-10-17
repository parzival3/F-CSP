package ac

import org.scalatest.flatspec.AnyFlatSpec

class TestSolver extends AnyFlatSpec {
  behavior of "Solver"

  private def simpleProblem2V = new {
    val var1: Variable = Variable("a")
    val var2: Variable = Variable("b")
    val c1: FunctionConstraint2V = FunctionConstraint2V(List(Variable("a"), Variable("b")), (a: Int, b: Int) => a > b)
    val directGraph: Map[FunctionConstraint2V, (Variable, Variable)] = Map((c1 -> ((var1, var2))))
    val listOfDomains = Map(
      Variable("a") -> Domain((0 to 10).toList),
      Variable("b") -> Domain((10 to 20).toList)
    )
    val csp = new CSP(List(var1,var2), listOfDomains, directGraph)
  }

  it should "be able to get a solution" in {
    val fixture = simpleProblem2V
    import fixture._
    val solution = Solution(csp, Assignments())
    print(solution.backtrack(solution).head.get.csp.domainMap)
  }
}
