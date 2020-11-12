package sv
import org.scalatest.flatspec.AnyFlatSpec
import sv.Random.{RandCInt, RandInt}
import scala.language.postfixOps

class Example2 extends AnyFlatSpec {
  behavior of "SystemVerilog"

  object pktType extends SVEnumeration {
    val UNICAST: Value = Value(11)
    val MULTICAST: Value = Value(0)
    val BROADCAST: Value = Value(1)
  }

  class Frame extends Random {
    import pktType._
    var pType: RandInt = rand(pType, pktType.domainValues())
    var len: RandInt = rand(len, 0 to 10 toList)
    var noRepeat: RandCInt = randc( noRepeat, 0 to 1 toList)
    var payload: RandInt = rand(payload, 0 to 7 toList)

    val common = constraintBlock (
      binary ((len, payload) => len == payload)
    )

    val unicast = constraintBlock(
      unary (len => len <= 2),
      unary (pType => pType == UNICAST.id)
    )

    val multicast = constraintBlock(
      unary (len => len >= 3),
      unary (len => len <= 4),
      unary (pType => pType == MULTICAST.id)
    )
  }

  import pktType._
  it should "be able to randomize a Frame" in {
    val frame = new Frame
    println("Disable MULTICAST")
    frame.multicast.disable()
    while (frame.randomize) {
      println(frame.debug())
      assert(frame.len <= 2)
      assert(frame.len == frame.payload)
      assert(frame.pType == UNICAST.id)
    }
    println("")
    println("Enable MULTICAST ---  Disable UNICAST")
    frame.unicast.disable()
    frame.multicast.enable()
    while (frame.randomize) {
      println(frame.debug())
      assert(frame.len <= 4 && frame.len >= 3)
      assert(frame.len == frame.payload)
      assert(frame.pType == MULTICAST.id)
    }

  }
}
