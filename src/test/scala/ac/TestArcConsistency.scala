package ac

import org.scalatest.flatspec.AnyFlatSpec

class TestArcConsistency extends AnyFlatSpec {
  behavior of "CSP"
  it should "be arc consistent" in {
    val listOfDomains = Map(Variable("a") -> Domain(0 to 10 toList), Variable("b") -> Domain(0 to 10 toList))
    val listOfVariables = List(Variable("a"), Variable("b"))
    val csp = new CSP(listOfVariables, listOfDomains)
    assert(csp.isArcConsistent)
  }
}
