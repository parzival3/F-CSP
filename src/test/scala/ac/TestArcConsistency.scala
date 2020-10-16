package ac

import org.scalatest.flatspec.AnyFlatSpec

class TestArcConsistency extends AnyFlatSpec {
  behavior of "Solution"
  it should "be arc consistent" in {
    val listOfDomains = Map(Variable("a") -> Domain(0 to 10 toList), Variable("b") -> Domain(0 to 10 toList))
    val listOfVariables = List(Variable("a"), Variable("b"))
    val constraintAB = Constraint((a, b) => a > b)
    val mapOfConstraint = Map((constraintAB -> ((Variable("a"), Variable("b")))))
    val csp = new CSP(listOfVariables, listOfDomains, mapOfConstraint)
    val solution = Solution(csp, Assignments())
    assert(solution.isArcConsistent)
  }

  it should "be also arc consistent" in {
    val listOfDomains = Map(Variable("a") -> Domain(0 to 10 toList), Variable("b") -> Domain(5 to 15 toList), Variable("c") -> Domain(5 to 15 toList))
    val listOfVariables = List(Variable("a"), Variable("b"), Variable("c"))
    val constraintAB = Constraint((a, b) => a > b)
    val constraintCB = Constraint((a, b) => a > b)
    val mapOfConstraint = Map((constraintAB -> ((Variable("a"), Variable("b")))), (constraintCB -> ((Variable("b"), Variable("c")))))
    val csp = new CSP(listOfVariables, listOfDomains, mapOfConstraint)
    val solution = Solution(csp, Assignments())
    assert(solution.isArcConsistent)
  }

  it should "not be arc consistent" in {
    val listOfDomains = Map(Variable("a") -> Domain(0 to 10 toList), Variable("b") -> Domain(10 to 20 toList))
    val listOfVariables = List(Variable("a"), Variable("b"))
    val constraint = Constraint((a, b) => a > b)
    val mapOfConstraint = Map((constraint -> ((Variable("a"), Variable("b")))), (constraint -> ((Variable("b"), Variable("c")))))
    val csp = new CSP(listOfVariables, listOfDomains, mapOfConstraint)
    val solution = Solution(csp, Assignments())
    assert(!solution.isArcConsistent)
  }
}
