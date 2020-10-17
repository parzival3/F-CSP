package ac

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
    val c1: FunctionConstraint2V = FunctionConstraint2V(List(Variable("a"), Variable("b")), (a: Int, b: Int) => a > b)
    val directGraph: Map[FunctionConstraint2V, (Variable, Variable)] = Map((c1 -> ((var1, var2))))
    val listOfDomains = Map(
      Variable("a") -> Domain((0 to 10).toList),
      Variable("b") -> Domain((10 to 20).toList)
    )
    val csp = new CSP(List(var1,var2), listOfDomains, directGraph)
  }

  behavior of "MAC"
  it should "be able to performa a MAC" in {
    import simpleProblem2V._
    val solution = Solution(csp, Assignments())
    print(solution.MAC().get.domainMap)
  }
}
