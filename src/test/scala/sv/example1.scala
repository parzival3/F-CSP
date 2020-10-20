package sv

import csp.{Assignments, Binary, CSP, Constraint, Domain, Node, Solution, Unary, Variable}
import org.scalatest.flatspec.AnyFlatSpec

class Random {

  implicit def tupleToUnary(a: Variable, fun: (Int) => Boolean): Unary = Unary(a, fun)
  implicit def toRange(a: Int): Range =  (a to a)

  type rar = Int
  var csp: Option[CSP] = None
  var randVars: List[(Variable, Domain)] = List[(Variable, Domain)]()
  var mapOfConstraint: List[Constraint] = List[Constraint]()
  var randCVars: List[(Variable, Iterator[Int])] = List[(Variable, Iterator[Int])]()
  var iterator: Option[Iterator[Solution with Node]] = None

  def rand(myMap: (String, Range)): Variable = {
    val addVar = Variable(myMap._1) -> Domain(myMap._2.toList)
    randVars = randVars ::: List(addVar)
    Variable(myMap._1)
  }

  def randc(myMap: (String, Range)): Variable = {
    val iter = LazyList.continually(myMap._2).flatten.iterator
    val addVar = Variable(myMap._1) -> iter
    randCVars = randCVars ::: List(addVar)
    Variable(myMap._1)
  }

  def randomize(): Option[Assignments] = {
    val cspAss = iterator match {
      case None => {
        csp = Some(new CSP(randVars.toMap.keySet.toList, randVars.toMap, mapOfConstraint))
        iterator = Some(Solution(csp.get, Assignments()).backtrackingSearch(csp.get).iterator)
        if (iterator.isEmpty) {
          None
        } else {
          Some(iterator.get.next().assignments)
        }
      }
      case Some(x) =>  if (x.isEmpty) None else Some(x.next().assignments)
    }
    val randcAss: List[Map[Variable, Int]] = randCVars.map(x => Map(x._1 -> x._2.next()))
    val z = randcAss.flatten.toMap
    cspAss match {
      case None => None
      case Some(x) => Some(Assignments(x.mapVarValue ++ z))
    }
  }
  def constraint(constraints: Constraint*): Unit = {
    mapOfConstraint = mapOfConstraint ::: constraints.toList
  }

}

class Frame extends  Random {
  var len: Variable = rand("len"-> (0 to 10))
  var noRepeat: Variable = randc("noRepeat"-> (0 to 1))
  var payload: Variable = rand("payload" -> (0 to 7))

  constraint (
    Unary(len , (len) => len >= 2),
    Unary(len, (len) =>  len <= 5),
    Binary(payload, len, (len, payload) => len == payload)
  )
}

class Example1 extends AnyFlatSpec {
  behavior of "SystemVerilog"

  it should "be able to randomize a Frame" in {
    val frame = new Frame
    var i = 0
    while ( i < 10) {
      val values = frame.randomize().get
      assert(values(frame.len) >= 2)
      assert(values(frame.len) <= 5)
      assert(values(frame.len) == values(frame.payload))
      i += 1
    }
  }
}
