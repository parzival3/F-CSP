// import constraint.SVMacros.RandInt
//import csp.Variable
import csp.Variable
import org.scalatest.flatspec.AnyFlatSpec
import sv.Random.RandInt

class TestMacros extends AnyFlatSpec {
  behavior of "SVMacros"
  class Hello extends sv.Random {
    var z: RandInt = rand (z, 0 to 10)

    val block = constraintBlock{
      un { z => z > 10 }
    }
  }

  it should "accept svrandc param" in {
    val o = new Hello
    assert(o.z == 0)
    assert(o.randVarsM.size == 1)
    assert(o.randVarsM.contains(Variable("z")))
    assert(o.mapOfConstraint.size == 1)
    assert(o.mapOfConstraint(0).isUnary)
    assert(o.mapOfConstraint(0).relatesToVar(Variable("z")))
  }

  it should "accpet domain param" in {

  }
}
