package sv
import csp._

import org.scalatest.flatspec.AnyFlatSpec

object pktType extends SVEnumeration {

  val UNICAST = Value(11)
  val MULTICAST = Value(0)
  val BROADCAST = Value(1)
}

class Frame extends Random {
  var pkType: Variable = randc_impl("pkType" -> pktType.domainValues())
  var len: Variable = rand_impl("len"-> (0 to 10).toList)
  var noRepeat: Variable = randc_impl("noRepeat"-> (0 to 1).toList)
  var payload: Variable = rand_impl("payload" -> (0 to 7).toList)

  val myBlock: ConstraintBlock = constraintBlock (
    Unary(len , (len) => len >= 2),
    Unary(len, (len) =>  len <= 5),
    Binary(payload, len, (len, payload) => len == payload)
  )
}

class Example1 extends AnyFlatSpec {
  behavior of "SystemVerilog"

  it should "be able to randomize a Frame" in {
    val frame = new Frame
    var randomFrame = frame.randomize()
    while (randomFrame.isDefined ) {
      val values = randomFrame.get
      println(values)
      assert(values(frame.len) >= 2)
      assert(values(frame.len) <= 5)
      assert(values(frame.len) == values(frame.payload))
      randomFrame = frame.randomize()
    }
  }
}
