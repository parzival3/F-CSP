package ac

import org.scalatest.flatspec.AnyFlatSpec

class TestSolver extends AnyFlatSpec {
  behavior of "Solver"

  private def simpleProblem2V = new {
    val var1: Variable = Variable("a")
    val var2: Variable = Variable("b")
    val c1: FunctionConstraint2V = FunctionConstraint2V(List(Variable("a"), Variable("b")), (a: Int, b: Int) => a >  b)
    val directGraph = List(c1)
    val listOfDomains = Map(
      Variable("a") -> Domain((0 to 10).toList),
      Variable("b") -> Domain((0 to 10).toList)
    )
    val csp = new CSP(List(var1,var2), listOfDomains, directGraph)
  }

  it should "be able to get a solution" in {
    val fixture = simpleProblem2V
    import fixture._
    val solution = Solution(csp, Assignments())
    val mySols = solution.backtrackingSearch(csp).iterator
    val firstSolution = mySols.next()
    assert(c1.fun(firstSolution.assignments.mapVarValue(var1), firstSolution.assignments.mapVarValue(var2)))
    val secondSolution = mySols.next()
    assert(c1.fun(secondSolution.assignments.mapVarValue(var1), secondSolution.assignments.mapVarValue(var2)))
  }
}
