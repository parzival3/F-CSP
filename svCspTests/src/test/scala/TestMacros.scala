// import constraint.SVMacros.RandInt
//import csp.Variable
import org.scalatest.flatspec.AnyFlatSpec
import sv.Random.{RandArray, RandInt}

class TestMacros extends AnyFlatSpec {
  behavior of "SVMacros"
  class Hello extends sv.Random {
    var z: RandInt = 10
    var l: RandInt = rand(l)
    var a: RandArray = rand(a)
    var d: RandInt = randd(d, 1 to 10)
    var p: Array[Int] = Array[Int]()
    p = Array(10, 12)
    svrandc(z)
    svrandc { p }
  }

  it should "accept svrandc param" in {
    val o = new Hello
    println(o.randVarsM)
    // o.randomMacro[Hello]
    println(o.z)
    println(o.l)
    println(o.a(0))
  }

  it should "accpet domain param" in {

  }
}
