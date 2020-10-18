package csp

import org.scalatest.flatspec.AnyFlatSpec

class TestSolver extends AnyFlatSpec {
  behavior.of("Solver")

  private def simpleProblem2V = new {
    val var1: Variable = Variable("a")
    val var2: Variable = Variable("b")
    val c1:   Binary = Binary(var1, var2, (a: Int, b: Int) => a > b)
    val c2:   Unary = Unary(var1, (a) => a > 3)
    val directGraph = List(c1, c2)
    val listOfDomains = Map(
      Variable("a") -> Domain((0 to 10).toList),
      Variable("b") -> Domain((0 to 10).toList)
    )
    val csp = new CSP(List(var1, var2), listOfDomains, directGraph)
  }

  it should "be able to get a solution" in {
    val fixture = simpleProblem2V
    import fixture._
    val solution = Solution(csp, Assignments())
    val mySols = solution.backtrackingSearch(csp).iterator
    val firstSolution = mySols.next()
    println(firstSolution)
    assert(c1.fun(firstSolution.assignments.mapVarValue(var1), firstSolution.assignments.mapVarValue(var2)))
    assert(c2.fun(firstSolution.assignments.mapVarValue(var1)))
    val secondSolution = mySols.next()
    println(secondSolution)
    assert(c1.fun(secondSolution.assignments.mapVarValue(var1), secondSolution.assignments.mapVarValue(var2)))
  }
}
