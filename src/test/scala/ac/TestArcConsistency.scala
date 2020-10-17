package ac

import org.scalatest.flatspec.AnyFlatSpec

class TestArcConsistency extends AnyFlatSpec {
  behavior of "Solution"

  private val varsConstraintFixture = new {
    val varA: Variable = Variable("a")
    val varB: Variable = Variable("b")
    val varC: Variable = Variable("c")
    val constraintAB: Binary = Binary(varA, varB, (a, b) => a > b)
    val constraintCB: Binary = Binary(varC, varB, (c, b) => c > b)
    val listOfABVars =  List(varA, varB)
    val listOfABCVars = List(varA, varB, varC)
    val mapOf2Constraint = List(constraintAB, constraintCB)
    val mapOf1Constraint = List(constraintAB)
  }

  it should "be arc consistent" in {
    import varsConstraintFixture._
    val listOfDomains = Map(Variable("a") -> Domain(0 to 10 toList), Variable("b") -> Domain(0 to 10 toList))
    val csp = new CSP(listOfABVars, listOfDomains, mapOf1Constraint)
    val solution = Solution(csp, Assignments())
    assert(solution.isArcConsistent(solution.csp))
  }

  it should "be also arc consistent" in {
    import varsConstraintFixture._
    val listOfDomains = Map(
      varA -> Domain(0 to 10 toList),
      varB -> Domain(5 to 15 toList),
      varC -> Domain(5 to 15 toList)
    )
    val csp = new CSP(listOfABCVars, listOfDomains, mapOf2Constraint)
    val solution = Solution(csp, Assignments())
    assert(solution.isArcConsistent(solution.csp))
  }

  it should "not be arc consistent" in {
    import varsConstraintFixture._

    val listOfDomains = Map(
      varA -> Domain(0 to 10 toList),
      varB -> Domain(10 to 20 toList),
      varC -> Domain(10 to 20 toList)
    )
    val csp = new CSP(listOfABCVars, listOfDomains, mapOf2Constraint)
    val solution = Solution(csp, Assignments())
    assert(!solution.isArcConsistent(solution.csp))
  }

  it should "not be arc consistent for mutli" in {
    import varsConstraintFixture._
    val squareConstraint = Binary(varA, varB, (a, b) => a * a == b)

    val listOfDomains = Map(
      varA -> Domain(100 to 110 toList),
      varB -> Domain(100 to 110 toList)
    )

    val csp = new CSP(listOfABVars, listOfDomains, List(squareConstraint))
    val solution = Solution(csp, Assignments())
    assert(!solution.isArcConsistent(solution.csp))
  }
}
