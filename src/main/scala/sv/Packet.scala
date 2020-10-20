package sv

import csp.{CSP, Constraint, Unary, Variable}
import constraint.Welcome

class Random {
  type rar = Int
  val csp: Option[CSP] = None
  var mapS: List[(Variable, Range)] = List[(Variable, Range)]()
  var mapOfConstraint: List[Constraint] = List[Constraint]()

  def rand(myMap: (Variable, Range)*): Unit = {
    mapS = mapS ::: myMap.toList
  }
  def randomize(): Random = {
    new Random
  }
  def constraint(constraints: Constraint*) = {
    mapOfConstraint = mapOfConstraint ::: constraints.toList
  }
}
case class NewDom(dom: List[Range]) {
  def inside_:(v: Variable): Constraint = {
    Unary(v, (a) => dom.flatMap(_.toList).contains(a))
  }
  def outside_:(v: Variable): Constraint = {
    Unary(v, (a) => !dom.flatMap(_.toList).contains(a))
  }
}

class Packet extends Random {
  val src = Variable("src")
  val dst = Variable("dst")
  val data = Variable("data")

  rand {
    src -> (0 to 31)
    dst -> (0 to 31)
    data -> (0 to 31)
  }

  constraint {
    src inside_: NewDom(List(0 to 10))
  }

}

object Main extends App {
  val x = 2
  val y = 3
  Welcome.isEvenLog(x + y)
}
