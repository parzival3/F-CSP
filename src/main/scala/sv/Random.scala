package sv

import csp.{Assignments, CSP, Constraint, Domain, Node, Solution, Unary, Variable}
import scala.collection.mutable.ListBuffer

class Random(val seed: Int = 42) {

  class ConstraintBlock(val constraints: List[Constraint], val r: Random) {
    def disable(): Unit = {
      r.removeConstraints(constraints)
      r.restartIterator()
    }

    def enable(): Unit = {
      r.addConstraints(constraints)
      r.restartIterator()
    }
  }

  type rar = Int
  var csp: Option[CSP] = None
  var randVars: List[(Variable, Domain)] = List[(Variable, Domain)]()
  var mapOfConstraint: ListBuffer[Constraint] = ListBuffer[Constraint]()
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

  def restartIterator(): Unit = {
    csp = Some(new CSP(randVars.toMap.keySet.toList, randVars.toMap, mapOfConstraint.toList))
    iterator = Some(Solution(csp.get, Assignments(), seed).backtrackingSearch(csp.get).iterator)
  }

  def randomize(): Option[Assignments] = {
    val cspAss = iterator match {
      case None => {
        restartIterator()
        if (iterator.isEmpty) None else Some(iterator.get.next().assignments)
      }
      case Some(x) =>  if (x.isEmpty) None else Some(x.next().assignments)
    }

    cspAss match {
      case None => None
      case Some(x) => Some(Assignments(x.mapVarValue ++ randCVars.flatMap(x => Map(x._1 -> x._2.next())).toMap))
    }
  }

  def constraintBlock(constraints: Constraint*): ConstraintBlock = {
    addConstraints(constraints.toList)
    new ConstraintBlock(constraints.toList, this)
  }

  def removeConstraints(constraints: List[Constraint]): Unit = {
    mapOfConstraint = mapOfConstraint --= constraints
  }

  def addConstraints(constraints: List[Constraint]): Unit = {
    mapOfConstraint = mapOfConstraint ++= constraints
  }
}
