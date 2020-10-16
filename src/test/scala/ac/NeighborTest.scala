package ac

import org.scalatest.flatspec.AnyFlatSpec

class NeighborTest extends AnyFlatSpec{
  behavior of "Neighbor"

  it should "be able to get all the Neighbor of a Variable" in {
    val var1 = Variable("a")
    val var2 = Variable("b")
    val var3 = Variable("c")
    val c1 = Constraint((a: Int, b: Int) => a > b)
    val c2 = Constraint((a: Int, b: Int) => a + b == 0)
    val directGraph: Map[Constraint, (Variable, Variable)] = Map((c1 -> ((var1, var2))), (c2 -> ((var2, var3))))



    val reversedGraph: Map[Variable, Map[Constraint, Variable]] = Map(
      (var1 -> Map(c1 -> var2)),
      (var2 -> Map((c1 -> var1), c2 -> var3)),
      (var3 -> Map((c2 -> var2))))

    val listOfDomains = Map(Variable("a") -> Domain((0 to 10).toList), Variable("b") -> Domain((10 to 20).toList))
    val csp = new CSP(List(var1,var2, var3), listOfDomains, directGraph)
    assert(reversedGraph == csp.reverseConstraintGraph(directGraph))
  }
}
