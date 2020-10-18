package csp

import org.scalatest.flatspec.AnyFlatSpec
/*
 * MAC (for Maintaining Arc Consistency (MAC))
 * After a variable Xi is assigned a value, the INFERENCE procedure calls AC-3,
 * but instead of a queue of all arcs in the CSP, we start with only the arcs (Xj,Xi) for all
 * Xj that are unassigned variables that are neighbors of Xi. From there, AC-3 does constraint
 * propagation in the usual way, and if any variable has its domain reduced to the empty set, the
 * call to AC-3 fails and we know to backtrack immediately. We can see that MAC is strictly
 * more powerful than forward checking because forward checking does the same thing as MAC
 * on the initial arcs in MAC’s queue; but unlike MAC, forward checking does not recursively
 * propagate constraints when changes are made to the domains of variables.
 */
class MACTest extends AnyFlatSpec {
  case class Assignment()

  private val simpleProblem2V = new {
    val var1: Variable = Variable("a")
    val var2: Variable = Variable("b")
    val c1:   Binary = Binary(var1, var2, (a: Int, b: Int) => a > b)
    val directGraph = List(c1)
    val listOfDomains = Map(
      Variable("a") -> Domain((0 to 10).toList),
      Variable("b") -> Domain((0 to 10).toList)
    )
    val csp = new CSP(List(var1, var2), listOfDomains, directGraph)
  }

  behavior.of("MAC")
  it should "be able to perform a MAC" in {
    import simpleProblem2V._
    val solution = Solution(csp, Assignments())
    val newDom = solution.MAC(solution.csp, List((var1, var2), (var2, var1))).get.varDomMap
    assert(newDom(var1).values == (1 to 10).toList)
    assert(newDom(var2).values == (0 to 9).toList)
  }

  behavior.of("MAC")
  it should "be able to perform a MAC2" in {
    import simpleProblem2V._
    val solution = Solution(csp, Assignments())
    val newDom = solution.MAC(solution.csp, List((var1, var2), (var2, var1))).get.varDomMap
    assert(newDom(var1).values == (1 to 10).toList)
    assert(newDom(var2).values == (0 to 9).toList)
  }
}
