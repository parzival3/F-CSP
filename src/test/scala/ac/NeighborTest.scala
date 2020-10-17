package ac

import org.scalatest.flatspec.AnyFlatSpec

class NeighborTest extends AnyFlatSpec{
  behavior of "Neighbor"

//  private def simpleProblem3V = new {
//    val var1: Variable = Variable("a")
//    val var2: Variable = Variable("b")
//    val var3: Variable = Variable("c")
//    val c1: FunctionConstraint2V = FunctionConstraint2V(List(Variable("a"), Variable("b")),(a: Int, b: Int) => a > b)
//    val c2: FunctionConstraint2V = FunctionConstraint2V(List(Variable("a"), Variable("b")), (a: Int, b: Int) => a + b == 0)
//    val directGraph =List(c1, c2)
//
//    val listOfDomains = Map(
//      Variable("a") -> Domain((0 to 10).toList),
//      Variable("b") -> Domain((0 to 10).toList),
//      Variable("c") -> Domain((0 to 10).toList))
//
//    val csp = new CSP(List(var1,var2, var3), listOfDomains, directGraph)
//  }


//  it should "be able to get all the Neighbor of a Variable" in {
//    val fixture = simpleProblem3V
//    import fixture._
//
////    val reversedGraph = Map(var1 -> Map(c1 -> var2)),
////      (var2 -> Map(c2 -> var3)),
////      (var3 -> Map())
////    )
//    // TODO: fix
//    // assert(reversedGraph == csp.reverseConstraintGraph)
//  }
//
//  it should "be able to get all the Neighbor of a Variable 2" in {
//    val fixture = simpleProblem3V
//    import fixture._
//
////    val reversedGraph: Map[Variable, Map[Variable, Iterable[FunctionConstraint2V]]] = Map(
////      (var1 -> Map(var2 -> Set(c1))),
////      (var2 -> Map(var3 -> Set(c2))),
////      (var3 -> Map()))
//    // TODO: fix
//    // assert(reversedGraph == csp.neighbors)
//  }

}
