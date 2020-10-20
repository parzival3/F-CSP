package sv


import csp.{Binary, Unary, Variable}
import org.scalatest.flatspec.AnyFlatSpec


class Frame extends Random {
  var len: Variable = rand("len"-> (0 to 10))
  var noRepeat: Variable = randc("noRepeat"-> (0 to 1))
  var payload: Variable = rand("payload" -> (0 to 7))

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
