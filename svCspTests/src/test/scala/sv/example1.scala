package sv
import org.scalatest.flatspec.AnyFlatSpec
import sv.Random.{RandCInt, RandInt}

import scala.language.postfixOps

class Example1 extends AnyFlatSpec {
  behavior of "SystemVerilog"

  object pktType extends SVEnumeration {
    val UNICAST = Value(11)
    val MULTICAST = Value(0)
    val BROADCAST = Value(1)
  }

  class Frame extends sv.Random(33) {
    var pkType: RandInt = rand(pkType, pktType.domainValues())
    var len: RandInt = rand(len, 0 to 10 toList)
    var noRepeat: RandCInt = randc(noRepeat, 0 to 1 toList)
    var payload: RandInt = rand(payload, 0 to 7 toList)

    val myBlock: ConstraintBlock = constraintBlock (
      unary ( len => len >= 2),
      unary (len =>  len <= 5),
      binary ((len, payload) => len == payload)
    )
  }

  it should "be able to randomize a Frame" in {
    val frame = new Frame
    while (frame.randomize) {
      println(frame.len)
      assert(frame.len >= 2)
      assert(frame.len <= 5)
      assert(frame.len == frame.payload)
    }
  }
}
