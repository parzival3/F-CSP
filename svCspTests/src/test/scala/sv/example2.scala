package sv
import csp._
import org.scalatest.flatspec.AnyFlatSpec


class Example2 extends AnyFlatSpec {
  behavior of "SystemVerilog"

  object pktType extends SVEnumeration {
    val UNICAST = Value(11)
    val MULTICAST = Value(0)
    val BROADCAST = Value(1)
  }

  class Frame extends Random {
    import pktType._
    var pType: Variable = rand_impl("pkType" -> pktType.domainValues())
    var len: Variable = rand_impl("len"-> (0 to 10).toList)
    var noRepeat: Variable = randc_impl("noRepeat"-> (0 to 1).toList)
    var payload: Variable = rand_impl("payload" -> (0 to 7).toList)

    val common: ConstraintBlock = constraintBlock (
      Binary(payload, len, (len, payload) => len == payload)
    )

    val unicast: ConstraintBlock = constraintBlock(
      Unary(len, len => len <= 2),
      Unary(pType, pType => pType == UNICAST.id)
    )

    val multicast: ConstraintBlock = constraintBlock(
      Unary(len, len => len >= 3),
      Unary(len, len => len <= 4),
      Unary(pType, pType => pType == MULTICAST.id)
    )
  }

  import pktType._
  it should "be able to randomize a Frame" in {
    val frame = new Frame

    println("Remove multicast")
    frame.multicast.disable()
    var randomFrame = frame.randomizeImp()
    while (randomFrame.isDefined ) {
      val values = randomFrame.get
      println(values)
      assert(values(frame.len) <= 2)
      assert(values(frame.len) == values(frame.payload))
      assert(values(frame.pType) == UNICAST.id)
      randomFrame = frame.randomizeImp()
    }

    println("Enable multicast Disable unicast")
    frame.unicast.disable()
    frame.multicast.enable()
    randomFrame = frame.randomizeImp()
    while (randomFrame.isDefined ) {
      val values = randomFrame.get
      println(values)
      assert(values(frame.len) <= 4 && values(frame.len) >= 3)
      assert(values(frame.len) == values(frame.payload))
      assert(values(frame.pType) == MULTICAST.id)
      randomFrame = frame.randomizeImp()
    }

  }
}
