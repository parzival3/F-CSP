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

  behavior of "MAC"
 // TODO: finish implementation of the test
//  it should "be able to performa a MAC" in {
//    val var1 = Variable("a")
//    val var2 = Variable("b")
//    val var3 = Variable("c")
//    val c1 = Constraint((a: Int, b: Int) => a > b)
//    val c2 = Constraint((a: Int, b: Int) => a + b == 0)
//    val ListOfUnasignedVariable = List(var2, var3)
//    val directGraph: Map[Constraint, (Variable, Variable)] = Map((c1 -> ((var1, var2))), (c2 -> ((var2, var3))))
//  }
}
