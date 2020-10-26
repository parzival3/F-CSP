// import constraint.SVMacros.RandInt
//import csp.Variable
import csp.Variable
import org.scalatest.flatspec.AnyFlatSpec
import sv.Random.RandInt

class TestMacros extends AnyFlatSpec {
  behavior of "SVMacros"
  class Hello extends sv.Random {
    var z: RandInt = rand (z, 0 to 10)
    var y: RandInt = rand (y, 0 to 10)
    var l: RandInt = rand (l, 0 to 20)
    val block = constraintBlock (
      unary { (z) => z > 3 },
      binary { (z, y) => z < y }
    )

  }

  it should "accept svrandc param" in {
    val o = new Hello
    assert(o.z == 0)
    o.randomizeImp()
    o.randomize
    assert(o.z < o.y)
    assert(o.z > 3)
    println(o.z)
    println(o.y)
    assert(o.randVarsM.size == 3)
    assert(o.randVarsM.contains(Variable("z")))
    assert(o.mapOfConstraint.size == 2)
    assert(o.mapOfConstraint(0).isUnary)
    assert(o.mapOfConstraint(0).relatesToVar(Variable("z")))
  }

  it should "accpet domain param" in {

  }
}
