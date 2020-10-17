package ac

case class Domain(values: List[Int])
case class Variable(name: String)


class CSP(val variables: List[Variable], val domainMap: Map[Variable, Domain], val constraints: List[Constraint]) {
//  def removeUnary: CSP = {
//    val newDomainMap = domainMap.map {x =>
//      val newDom = x._2.values.filter { value =>
//         constraints.filter(c => c.isUnary && c.relatesToVar(x._1))
//         .forall(cc => cc.isSatisfied(Map(x._1 -> value)))
//      }
//      (x._1, Domain(newDom))
//    }
//    new CSP(variables, newDomainMap, constraints)
//  }

  def restrictDomain(mapVarDomain: (Variable, Domain)): CSP = {
    val newDomain = (domainMap - mapVarDomain._1)  + mapVarDomain
    new CSP(variables, newDomain, constraints)
  }

  /*
     * Get all the constraint between two variables
     * @param variables List[Variable]
     * @return List(Constraint)
    */
  def getConstraints(variables: List[Variable]): List[Constraint] = {
    constraints.filter(c => c.relatesTo(variables))
  }

  def reviseArcs(Xi: Variable, Xj: Variable): List[(Variable, Variable)] = {
    constraints.filter(c => !c.isUnary)
    .filter(c => c.relatesToVar(Xi) && !c.relatesToVar(Xj)).map(c => (c.getOther(Xi).get, Xi)).distinct
  }

  def neighbors(variable: Variable): List[Variable] = {
    constraints.filter(c => c.relatesToVar(variable)).flatMap(c => c.neighbor.filterNot(_ == variable)).distinct
  }

  val combinationOfNeighbors: List[(Variable, Variable)] =
    constraints.filterNot(_.isUnary)
    .flatMap(c => List((c.neighbor.head, c.neighbor(1)), (c.neighbor(1), c.neighbor.head))).distinct
}
