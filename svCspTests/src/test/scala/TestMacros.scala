// import constraint.SVMacros.RandInt
//import csp.Variable
import csp.Variable
import org.scalatest.flatspec.AnyFlatSpec
import sv.Random.{RandCInt, RandInt}

class TestMacros extends AnyFlatSpec {
  behavior of "SVMacros"

  it should "accept svrandc param" in {

    class Hello extends sv.Random {
      var z: RandInt = rand (z, 0 to 10)
      var y: RandInt = rand (y, 0 to 10)
      var l: RandInt = rand (l, 0 to 20)
      val block = constraintBlock (
        unary { (z) => z > 3 },
        binary { (z, y) => z < y }
      )

    }

    val o = new Hello
    assert(o.z == 0)
    assert(o.randVarsM.size == 3)
    assert(o.randVarsM.contains(Variable("l")))
    assert(o.randVarsM.contains(Variable("z")))
    assert(o.mapOfConstraint.size == 2)
    assert(o.mapOfConstraint(0).isUnary)
    assert(o.mapOfConstraint(0).relatesToVar(Variable("z")))
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

  it should "be able to add randc variables" in {
    class ClassWithRandC extends sv.Random {
      var randCvar: RandCInt = randc (randCvar, 1 to 10)
      var randvar: RandCInt = randc (randvar, 1 to 23)
    }

    val myClass = new ClassWithRandC
    assert(myClass.randCVarsM.size == 2)
    assert(myClass.randCVarsM.contains(Variable("randCvar")))
    assert(myClass.randCvar == 0)
    myClass.randomize
    println(myClass.randCvar)
    println(myClass.randvar)
    myClass.randomize
    println(myClass.randCvar)
    println(myClass.randvar)
  }
}
